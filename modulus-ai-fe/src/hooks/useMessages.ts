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

    // Helper function to merge messages and remove duplicates based on chatMessageID
    const mergeMessages = (existingMessages: ChatMessageRead[], newMessages: ChatMessageRead[]): ChatMessageRead[] => {
        const messageMap = new Map<number, ChatMessageRead>();
        
        // Add existing messages to map
        existingMessages.forEach(msg => {
            messageMap.set(msg.chatMessageID, msg);
        });
        
        // Add or update with new messages
        newMessages.forEach(msg => {
            messageMap.set(msg.chatMessageID, msg);
        });
        
        // Convert back to array and sort by chatMessageID
        // Temporary messages (negative IDs) should come after real messages
        return Array.from(messageMap.values()).sort((a, b) => {
            // If both are temporary (negative), sort by ID
            if (a.chatMessageID < 0 && b.chatMessageID < 0) {
                return b.chatMessageID - a.chatMessageID; // Reverse for negative (newer first)
            }
            // If only one is temporary, real messages come first
            if (a.chatMessageID < 0) return 1;
            if (b.chatMessageID < 0) return -1;
            // Both are real messages, sort normally
            return a.chatMessageID - b.chatMessageID;
        });
    };

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
                // Reverse messages so newest are at bottom (oldest first) and merge to ensure uniqueness
                setMessages(mergeMessages([], msgs.reverse()));
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
                // Merge older messages with existing ones, ensuring no duplicates
                setMessages(prev => mergeMessages(prev, olderMsgs.reverse()));
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
            // Merge with existing messages to avoid duplicates
            setMessages(prev => mergeMessages(prev, updatedMsgs.reverse()));
        } catch (err) {
            console.error("Failed to send message", err);
        }

        const assistantMessageId = crypto.randomUUID();
        const assistantMessage: ChatMessageRead = {
            chatMessageID: -Date.now(), // Use negative timestamp to avoid conflicts with real IDs
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
            
            // Reload messages to get the real assistant message with proper ID
            const updatedMsgs = await apiService.getMessages(
                conversation.conversationPublicID,
                undefined,
                navigate,
            );
            // Remove temporary message and merge with real messages
            setMessages(prev => {
                const filteredPrev = prev.filter(m => m.chatMessagePublicID !== assistantMessageId);
                return mergeMessages(filteredPrev, updatedMsgs.reverse());
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
