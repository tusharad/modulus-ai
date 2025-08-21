import React from 'react';
import type { ChatMessageRead } from '../../types';

interface Props {
  message: ChatMessageRead;
}

const MessageBubble: React.FC<Props> = ({ message }) => {
  const isUser = message.chatMessageRole === 'MessageRoleUser';
  return (
    <div className={`flex ${isUser ? 'justify-end' : 'justify-start'}`}>
      <div
        className={`px-4 py-2 rounded-lg max-w-xs ${
          isUser
            ? 'bg-blue-600 text-white rounded-br-none'
            : 'bg-gray-200 text-gray-900 rounded-bl-none'
        }`}
      >
        {message.chatMessageContent}
      </div>
    </div>
  );
};

export default MessageBubble;
