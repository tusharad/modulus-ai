import React from 'react';
import type { ChatMessageRead } from '../../types';
import MessageBubble from './MessageBubble';

interface Props {
  messages: ChatMessageRead[];
}

const MessageList: React.FC<Props> = ({ messages }) => (
  <div className="flex-1 overflow-y-auto p-4 space-y-4">
    {messages.map((msg) => (
      <MessageBubble key={msg.chatMessagePublicID} message={msg} />
    ))}
  </div>
);

export default MessageList;
