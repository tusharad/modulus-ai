import type { AddConversationRequest, AddMessageRequest, AuthTokens, ChatMessageRead, ConversationRead, LoginRequest, RegisterRequest, UserProfile } from '../types';
import { getCookie } from './cookies';

// API service
class APIService {
    private baseUrl = 'http://localhost:8081/api/v1';
    
    private async fetchWithAuth(url: string, options: RequestInit = {}) {
      const token = getCookie('accessToken');
      return fetch(url, {
        ...options,
        headers: {
          'Content-Type': 'application/json',
          ...(token && { 'Authorization': `Bearer ${token}` }),
          ...options.headers,
        },
      });
    }
  
    async register(data: RegisterRequest): Promise<UserProfile> {
      const response = await fetch(`${this.baseUrl}/auth/register`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      });
      if (!response.ok) throw new Error('Registration failed');
      return response.json();
    }
  
    async login(data: LoginRequest): Promise<AuthTokens> {
      const response = await fetch(`${this.baseUrl}/auth/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      });
      if (!response.ok) throw new Error('Login failed');
      return response.json();
    }
  
    async getConversations(): Promise<ConversationRead[]> {
      const response = await this.fetchWithAuth(`${this.baseUrl}/conversations`);
      if (!response.ok) throw new Error('Failed to fetch conversations');
      return response.json();
    }
  
    async createConversation(data: AddConversationRequest): Promise<{ id: string }> {
      const response = await this.fetchWithAuth(`${this.baseUrl}/conversations`, {
        method: 'POST',
        body: JSON.stringify(data),
      });
      if (!response.ok) throw new Error('Failed to create conversation');
      return response.json();
    }
  
    async getMessages(conversationId: string): Promise<ChatMessageRead[]> {
      const response = await this.fetchWithAuth(`${this.baseUrl}/conversations/${conversationId}`);
      if (!response.ok) throw new Error('Failed to fetch messages');
      return response.json();
    }
  
    async sendMessage(conversationId: string, data: AddMessageRequest): Promise<void> {
      const response = await this.fetchWithAuth(`${this.baseUrl}/conversations/${conversationId}`, {
        method: 'POST',
        body: JSON.stringify(data),
      });
      if (!response.ok) throw new Error('Failed to send message');
    }
  
    async deleteConversation(conversationId: string): Promise<void> {
      const response = await this.fetchWithAuth(`${this.baseUrl}/conversations/${conversationId}`, {
        method: 'DELETE',
      });
      if (!response.ok) throw new Error('Failed to delete conversation');
    }
  }

export const apiService = new APIService();