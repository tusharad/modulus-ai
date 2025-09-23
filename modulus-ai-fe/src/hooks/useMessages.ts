import { useEffect, useState } from "react";
import { apiService } from "../services/api.service";
import type { ChatMessageRead, ConversationRead } from "../types";
import { useNavigate } from "react-router";

export const useMessages = (conversation: ConversationRead | null) => {
    const [messages, setMessages] = useState<ChatMessageRead[]>([]);
    const [loading, setLoading] = useState(false);
    const [loadingMore, setLoadingMore] = useState(false);
    const [hasMoreMessages, setHasMoreMessages] = useState(true);
    const navigate = useNavigate();

    // Load messages when conversation changes
    useEffect(() => {
        if (!conversation) {
            setMessages([]);
            setHasMoreMessages(true);
            return;
        }

        const loadMessages = async () => {
            setLoading(true);
            try {
                const msgs = await apiService.getMessages(
                    conversation.conversationPublicID,
                    undefined,
                    navigate,
                );
                // Reverse messages so newest are at bottom (oldest first)
                setMessages(msgs.reverse());
                // If we get fewer messages than expected, there might not be more
                setHasMoreMessages(msgs.length > 0);
            } catch (err) {
                console.error("Failed to load messages", err);
            } finally {
                setLoading(false);
            }
        };

        loadMessages();
    }, [conversation, navigate]);

    // Load more messages (older messages)
    const loadMoreMessages = async () => {
        if (!conversation || loadingMore || !hasMoreMessages || messages.length === 0) {
            return;
        }

        setLoadingMore(true);
        try {
            // Get the oldest message ID (since messages are now in asc order, it's the first one)
            const oldestMessageId = messages[0].chatMessageID;
            const olderMsgs = await apiService.getMessages(
                conversation.conversationPublicID,
                oldestMessageId,
                navigate,
            );
            
            if (olderMsgs.length === 0) {
                setHasMoreMessages(false);
            } else {
                // Prepend older messages (reverse them first since API returns desc order)
                setMessages(prev => [...olderMsgs.reverse(), ...prev]);
            }
        } catch (err) {
            console.error("Failed to load more messages", err);
        } finally {
            setLoadingMore(false);
        }
    };

    const sendMessage = async (
        content: string,
        llmConfig: { provider: string; model: string; apiKey?: string, toolCall?: "Wikipedia" | "WebSearch" | null},
        file?: File
    ) => {
        if (!conversation) return;
        try {
            await apiService.sendMessage(conversation.conversationPublicID, {
                messageContent: content,
                addMessageRole: "user",
                file,
            });
            const updatedMsgs = await apiService.getMessages(
                conversation.conversationPublicID,
                undefined,
                navigate,
            );
            // Reverse messages so newest are at bottom (oldest first)
            setMessages(updatedMsgs.reverse());
        } catch (err) {
            console.error("Failed to send message", err);
        }

        const assistantMessageId = crypto.randomUUID();
        const assistantMessage: ChatMessageRead = {
            chatMessageID: 0,
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
                    ...(llmConfig.apiKey && { apiKey: llmConfig.apiKey }),
                    ...(llmConfig.toolCall && { toolCall: llmConfig.toolCall })
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

    return { 
        messages, 
        sendMessage, 
        loading, 
        loadingMore, 
        hasMoreMessages, 
        loadMoreMessages 
    };
};
