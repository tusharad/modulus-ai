import React, { useState } from "react";
import { deleteCookie } from "../../services/cookies";
import ConversationList from "./ConversationList";
import MessageList from "./MessageList";
import MessageInput from "./MessageInput";
import { useConversations } from "../../hooks/useConversations";
import { useMessages } from "../../hooks/useMessages";
import { useNavigate } from "react-router";
import TopBar from "./TopBar";

const ChatPage: React.FC = () => {
    const navigate = useNavigate();
    const {
        conversations,
        selectedConversation,
        setSelectedConversation,
        createConversation,
        deleteConversation,
    } = useConversations();

    const { messages, sendMessage } = useMessages(selectedConversation);
    const [llmConfig, setLlmConfig] = useState<
        {
            provider: string;
            model: string;
            apiKey?: string;
            toolCall?: "Wikipedia" | "WebSearch" | null;
        } | null
    >(null);

    const [sidebarCollapsed, setSidebarCollapsed] = useState(false);
    const handleLogout = () => {
        deleteCookie("accessToken");
        deleteCookie("refreshToken");
        navigate("/login");
    };

    return (
        <div className="chat-container">
            <TopBar
                onChange={(config) =>
                    setLlmConfig((prev) => ({ ...prev, ...config }))}
                onLogout={handleLogout}
            />

            <div className="flex flex-1 overflow-hidden">
                <ConversationList
                    conversations={conversations}
                    selectedConversation={selectedConversation}
                    onSelect={setSelectedConversation}
                    onDelete={deleteConversation}
                    onNewConversation={() => createConversation()}
                    onLogout={handleLogout}
                    collapsed={sidebarCollapsed}
                    onToggleCollapse={() => setSidebarCollapsed((c) => !c)}
                />

                <div className="flex flex-col flex-1 bg-gradient-to-br from-gray-50 to-gray-100">
                    {selectedConversation
                        ? (
                            <>
                                <MessageList messages={messages} />
                                <MessageInput
                                    onSend={(content, file) => {
                                        if (llmConfig) {
                                            sendMessage(
                                                content,
                                                llmConfig,
                                                file,
                                            );
                                        }
                                    }}
                                    toolCall={llmConfig?.toolCall || null}
                                    setToolCall={(tool) =>
                                        setLlmConfig((prev) => (prev
                                            ? { ...prev, toolCall: tool }
                                            : prev)
                                        )}
                                />
                            </>
                        )
                        : (
                            <div className="flex flex-col items-center justify-center flex-1 text-gray-500 p-8">
                                <div className="text-center max-w-md">
                                    <div className="w-24 h-24 mx-auto mb-6 bg-gradient-to-br from-blue-100 to-indigo-100 rounded-2xl flex items-center justify-center">
                                        <svg className="w-12 h-12 text-blue-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                                        </svg>
                                    </div>
                                    <h3 className="text-xl font-semibold text-gray-700 mb-2">Welcome to AI Chat</h3>
                                    <p className="text-gray-500 text-sm mb-6">
                                        Select a conversation from the sidebar or create a new one to start chatting with AI
                                    </p>
                                    <button
                                        onClick={() => createConversation()}
                                        className="btn btn-primary px-6 py-3 rounded-xl"
                                    >
                                        Start New Conversation
                                    </button>
                                </div>
                            </div>
                        )}
                </div>
            </div>
        </div>
    );
};

export default ChatPage;
