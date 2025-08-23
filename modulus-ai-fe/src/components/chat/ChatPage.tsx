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
        { provider: string; model: string; apiKey?: string } | null
    >(null);

    
    const [sidebarCollapsed, setSidebarCollapsed] = useState(false);
    const handleLogout = () => {
        deleteCookie("accessToken");
        deleteCookie("refreshToken");
        navigate("/login");
    };

    return (
    <div className="flex h-screen flex-col">
      <TopBar onChange={setLlmConfig} onLogout={handleLogout} />

      <div className="flex flex-1">
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

        <div className="flex flex-col flex-1 bg-gray-50">
          {selectedConversation ? (
            <>
              <MessageList messages={messages} />
              <MessageInput onSend={(content, file) => {
                if (llmConfig) {
                  sendMessage(content, llmConfig, file);
                }
              }} />
            </>
          ) : (
            <div className="flex items-center justify-center flex-1 text-gray-500">
              Select or create a conversation
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default ChatPage;
