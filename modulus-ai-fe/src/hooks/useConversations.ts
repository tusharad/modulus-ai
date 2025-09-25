import { useState, useEffect } from 'react';
import { apiService } from '../services/api.service';
import type { ConversationRead } from '../types';

export const useConversations = () => {
  const [conversations, setConversations] = useState<ConversationRead[]>([]);
  const [selectedConversation, setSelectedConversation] = useState<ConversationRead | null>(null);
  const [loadingConversation, setLoadingConversation] = useState(true);

  // Load conversations on mount
  useEffect(() => {
    const loadConversations = async () => {
      try {
        const data = await apiService.getConversations();
        setConversations(data);
        if (data.length > 0) setSelectedConversation(data[0]);
      } catch (err) {
        console.error('Failed to load conversations', err);
      } finally {
        setLoadingConversation(false);
      }
    };
    loadConversations();
  }, []);

  const createConversation = async (title: string = 'New Chat') => {
    try {
      const newConvPubId = await apiService.createConversation({ conversationTitle: title });
      const updatedConvs : ConversationRead[] = await apiService.getConversations();
      setConversations(updatedConvs);
      const conv = updatedConvs.find((c) => c.conversationPublicID === newConvPubId);
      setSelectedConversation(conv);
    } catch (err) {
      console.error('Failed to create conversation', err);
    }
  };

  const deleteConversation = async (id: string) => {
    try {
      await apiService.deleteConversation(id);
      const updatedConvs = await apiService.getConversations();
      setConversations(updatedConvs);
      if (selectedConversation?.conversationPublicID === id) {
        setSelectedConversation(updatedConvs[0] || null);
      }
    } catch (err) {
      console.error('Failed to delete conversation', err);
    }
  };

  return {
    conversations,
    selectedConversation,
    setSelectedConversation,
    createConversation,
    deleteConversation,
    loadingConversation,
  };
};
