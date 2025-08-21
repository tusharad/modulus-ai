import { useState, useEffect } from 'react';
import { apiService } from '../services/api.service';
import type { ChatMessageRead, ConversationRead } from '../types';

export const useMessages = (conversation: ConversationRead | null) => {
  const [messages, setMessages] = useState<ChatMessageRead[]>([]);
  const [loading, setLoading] = useState(false);

  // Load messages when conversation changes
  useEffect(() => {
    if (!conversation) {
      setMessages([]);
      return;
    }

    const loadMessages = async () => {
      setLoading(true);
      try {
        const msgs = await apiService.getMessages(conversation.conversationPublicID);
        setMessages(msgs);
      } catch (err) {
        console.error('Failed to load messages', err);
      } finally {
        setLoading(false);
      }
    };

    loadMessages();
  }, [conversation]);

  const sendMessage = async (content: string) => {
    if (!conversation) return;
    try {
      await apiService.sendMessage(conversation.conversationPublicID, {
        messageContent: content,
        addMessageRole: 'user',
      });
      const updatedMsgs = await apiService.getMessages(conversation.conversationPublicID);
      setMessages(updatedMsgs);
    } catch (err) {
      console.error('Failed to send message', err);
    }
  };

  return { messages, sendMessage, loading };
};
