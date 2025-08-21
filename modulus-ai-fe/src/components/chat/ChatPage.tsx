import React from 'react';
import { deleteCookie } from '../../services/cookies';
import ConversationList from './ConversationList';
import MessageList from './MessageList';
import MessageInput from './MessageInput';
import { useConversations } from '../../hooks/useConversations';
import { useMessages } from '../../hooks/useMessages';
import { useNavigate } from 'react-router';

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

  const handleLogout = () => {
    deleteCookie('accessToken');
    deleteCookie('refreshToken');
    navigate('/login');
  };

  return (
    <div className="flex h-screen">
      <ConversationList
        conversations={conversations}
        selectedConversation={selectedConversation}
        onSelect={setSelectedConversation}
        onDelete={deleteConversation}
        onNewConversation={() => createConversation()}
        onLogout={handleLogout}
      />

      <div className="flex flex-col flex-1 bg-gray-50">
        {selectedConversation ? (
          <>
            <MessageList messages={messages} />
            <MessageInput onSend={sendMessage} />
          </>
        ) : (
          <div className="flex items-center justify-center flex-1 text-gray-500">
            Select or create a conversation
          </div>
        )}
      </div>
    </div>
  );
};

export default ChatPage;
