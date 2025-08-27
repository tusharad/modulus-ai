import type {
    AddConversationRequest,
    AddMessageRequest,
    AuthTokens,
    ChatMessageRead,
    ChatMessageWithAttachments,
    ConversationRead,
    LoginRequest,
    RegisterRequest,
    UserProfile,
} from "../types";
import { getCookie } from "./cookies";
import { deleteCookie, setCookie } from "./cookies.ts";

// API service
class APIService {
    private baseUrl = import.meta.env.VITE_API_BASE_URL ?? "http://localhost:8081/api/v1";

    private async refreshTokens(): Promise<boolean> {
        const refreshToken = getCookie("refreshToken");
        if (!refreshToken) return false;

        try {
            const resp = await fetch(`${this.baseUrl}/auth/refresh-token`, {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ refreshToken }),
            });

            if (!resp.ok) return false;

            const tokens: AuthTokens = await resp.json();
            setCookie("accessToken", tokens.accessToken);
            setCookie("refreshToken", tokens.refreshToken);
            return true;
        } catch (err) {
            console.error("Failed to refresh token", err);
            return false;
        }
    }

    private async fetchWithAuth(
        url: string,
        options: RequestInit = {},
        navigate?: (path: string) => void,
    ): Promise<Response> {
        const token = getCookie("accessToken");
        const headers: Record<string, string> = {
            ...(options.headers as Record<string, string>),
            ...(token && { Authorization: `Bearer ${token}` }),
        };

        // ❌ Don't force Content-Type if FormData is used
        if (!(options.body instanceof FormData)) {
            headers["Content-Type"] = "application/json";
        }

        let response = await fetch(url, {
            ...options,
            headers,
        });

        if (response.status === 401) {
            const refreshed = await this.refreshTokens();
            if (!refreshed) {
                // No refresh token → force logout
                deleteCookie("accessToken");
                deleteCookie("refreshToken");
                if (navigate) navigate("/login");
                throw new Error("Unauthorized - redirecting to login");
            }

            // Retry once with new token
            const newToken = getCookie("accessToken");
            const retryHeaders: Record<string, string> = {
                ...(options.headers as Record<string, string>),
                ...(newToken && { Authorization: `Bearer ${newToken}` }),
            };
            if (!(options.body instanceof FormData)) {
                retryHeaders["Content-Type"] = "application/json";
            }

            response = await fetch(url, {
                ...options,
                headers: retryHeaders,
            });
        }

        return response;
    }

    async register(data: RegisterRequest): Promise<UserProfile> {
        const response = await fetch(`${this.baseUrl}/auth/register`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(data),
        });
        if (!response.ok) throw new Error("Registration failed");
        return response.json();
    }

    async login(data: LoginRequest): Promise<AuthTokens> {
        const response = await fetch(`${this.baseUrl}/auth/login`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(data),
        });
        if (!response.ok) throw new Error("Login failed");
        return response.json();
    }

    async getConversations(): Promise<ConversationRead[]> {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations`,
        );
        if (!response.ok) throw new Error("Failed to fetch conversations");
        return response.json();
    }

    async createConversation(
        data: AddConversationRequest,
    ): Promise<{ id: string }> {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations`,
            {
                method: "POST",
                body: JSON.stringify(data),
            },
        );
        if (!response.ok) throw new Error("Failed to create conversation");
        return response.json();
    }

    async getMessages(
        conversationId: string,
        navigate?: (path: string) => void,
    ): Promise<ChatMessageRead[]> {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations/${conversationId}`,
            {},
            navigate,
        );
        if (!response.ok) throw new Error("Failed to fetch messages");
        const data: ChatMessageWithAttachments[] = await response.json();
        return data.map((item) => item.cm);
    }

    async sendMessage(
        conversationId: string,
        data: {
            messageContent: string;
            addMessageRole: "user" | "assistant";
            file?: File;
        },
    ): Promise<void> {
        const formData = new FormData();
        formData.append("messageContent", data.messageContent);
        formData.append("addMessageRole", data.addMessageRole);

        if (data.file) {
            formData.append("addMessageAttachment", data.file);
        }

        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations/${conversationId}`,
            {
                method: "POST",
                body: formData,
            },
        );

        if (!response.ok) throw new Error("Failed to send message");
    }

    async deleteConversation(conversationId: string): Promise<void> {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations/${conversationId}`,
            {
                method: "DELETE",
            },
        );
        if (!response.ok) throw new Error("Failed to delete conversation");
    }


    async verifyOTP(
        data: { verifyEmail: string; verifyOTP: number },
    ): Promise<string> {
        const response = await fetch(`${this.baseUrl}/auth/verify-otp`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(data),
        });
        if (!response.ok) throw new Error("OTP verification failed");
        return response.text(); // server returns Text
    }

    async streamMessage(
        conversationId: string,
        body: { modelUsed: string; provider: string },
        onChunk: (content: string) => void,
    ): Promise<void> {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations/${conversationId}/stream`,
            {
                method: "POST",
                body: JSON.stringify(body),
            },
        );

        if (!response.ok || !response.body) throw new Error("Failed to stream");

        const reader = response.body.getReader();
        const decoder = new TextDecoder();

        while (true) {
            const { done, value } = await reader.read();
            if (done) break;

            const chunk = decoder.decode(value, { stream: true });

            // Each line is a JSON object
            chunk.split("\n").forEach((line) => {
                if (!line.trim()) return;
                try {
                    const data = JSON.parse(line);
                    if (data.respContent) {
                        onChunk(data.respContent);
                    }
                } catch {
                    console.warn("Skipping bad chunk:", line);
                }
            });
        }
    }

    async getModelProviders(): Promise<
        {
            isApiFieldRequired: boolean;
            modelList: string[];
            providerName: string;
        }[]
    > {
        const response = await this.fetchWithAuth(
            `${this.baseUrl}/conversations/model_providers`,
        );
        if (!response.ok) throw new Error("Failed to fetch model providers");
        return response.json();
    }
}

export const apiService = new APIService();
