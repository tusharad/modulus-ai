import { useEffect, useState } from "react";
import { apiService } from "../services/api.service";
import type { ChatMessageRead, ConversationRead } from "../types";
import { useNavigate } from "react-router";

export const useMessages = (conversation: ConversationRead | null) => {
    const [messages, setMessages] = useState<ChatMessageRead[]>([]);
    const [loading, setLoading] = useState(false);
    const navigate = useNavigate();

    // Load messages when conversation changes
    useEffect(() => {
        if (!conversation) {
            setMessages([]);
            return;
        }

        const loadMessages = async () => {
            setLoading(true);
            try {
                const msgs = await apiService.getMessages(
                    conversation.conversationPublicID,
                    navigate,
                );
                setMessages(msgs);
            } catch (err) {
                console.error("Failed to load messages", err);
            } finally {
                setLoading(false);
            }
        };

        loadMessages();
    }, [conversation, navigate]);

    const sendMessage = async (
        content: string,
        llmConfig: { provider: string; model: string; apiKey?: string },
        file?: File
    ) => {
        if (!conversation) return;
        try {
            await apiService.sendMessage(conversation.conversationPublicID, {
                messageContent: content,
                addMessageRole: "user",
                file
            });
            const updatedMsgs = await apiService.getMessages(
                conversation.conversationPublicID,
            );
            setMessages(updatedMsgs);
        } catch (err) {
            console.error("Failed to send message", err);
        }

        const assistantMessageId = crypto.randomUUID();
        const assistantMessage: ChatMessageRead = {
            chatMessagePublicID: assistantMessageId,
            chatMessageContent: "loading...",
            chatMessageRole: "MessageRoleAssistant",
            chatMessageCreatedAt: new Date().toISOString(),
        };
        setMessages((prev) => [...prev, assistantMessage]);

        try {
            // Start streaming assistant response
            let finalContent = "";
            await apiService.streamMessage(
                conversation.conversationPublicID,
                {
                    modelUsed: llmConfig.model,
                    provider: llmConfig.provider,
                    apiKey: llmConfig.apiKey,
                },
                (chunk: string) => {
                    finalContent += chunk;
                    // Update assistant bubble progressively
                    setMessages((prev) =>
                        prev.map((m) =>
                            m.chatMessagePublicID === assistantMessageId
                                ? { ...m, chatMessageContent: finalContent }
                                : m
                        )
                    );
                },
            );

            // Once streaming ends, persist assistant message
            await apiService.sendMessage(conversation.conversationPublicID, {
                messageContent: finalContent,
                addMessageRole: "assistant",
            });
        } catch (err) {
            console.error("Streaming failed:", err);
            // Update assistant bubble to error state
            setMessages((prev) =>
                prev.map((m) =>
                    m.chatMessagePublicID === assistantMessageId
                        ? {
                            ...m,
                            chatMessageContent: "[Error: failed to stream]",
                        }
                        : m
                )
            );
        }
    };

    return { messages, sendMessage, loading };
};
