
export interface ChatMessageRead {
    chatMessagePublicID: string;
    chatMessageContent: string;
    chatMessageRole: 'MessageRoleUser' | 'MessageRoleAssistant';
    chatMessageCreatedAt: string;
  }
  
  export interface ConversationRead {
    conversationPublicID: string;
    conversationTitle: string;
    conversationCreatedAt: string;
  }
  
  export interface AddConversationRequest {
    conversationTitle: string;
  }
  
  export interface AddMessageRequest {
    messageContent: string;
    addMessageRole: 'user' | 'assistant';
    addMessageProvider?: string;
    addMessageModel?: string;
  }
  
  export interface LLMRespStreamBody {
    modelUsed: string;
    provider: string;
    apiKey?: string;
  }
