export interface ChatMessageRead {
    chatMessageID: number;
    chatMessagePublicID: string;
    chatMessageContent: string;
    chatMessageRole: "MessageRoleUser" | "MessageRoleAssistant";
    chatMessageCreatedAt: string;
}

export interface MessageAttachmentRead {
    messageAttachmentID: string;
    messageAttachmentMessageID: string;
    messageAttachmentFileName: string;
    messageAttachmentFileType: string;
    messageAttachmentFileSizeBytes: number;
    messageAttachmentStoragePath: string;
    messageAttachmentCreatedAt: string;
}

export interface ChatMessageWithAttachments {
    cm: ChatMessageRead;
    mas: [MessageAttachmentRead];
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
    addMessageRole: "user" | "assistant";
    addMessageProvider?: string;
    addMessageModel?: string;
}

export interface LLMRespStreamBody {
    modelUsed: string;
    provider: string;
    apiKey?: string;
}

export interface LLMConfig {
    provider: string;
    model: string;
    apiKey?: string;
    toolCall?: "Wikipedia" | "WebSearch"; 
}
