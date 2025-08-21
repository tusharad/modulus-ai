import React from 'react';
import { X } from 'lucide-react';
import type { ConversationRead } from '../../types';

interface Props {
  conversation: ConversationRead;
  selected: boolean;
  onClick: () => void;
  onDelete: () => void;
}

const ConversationItem: React.FC<Props> = ({ conversation, selected, onClick, onDelete }) => (
  <div
    className={`flex items-center justify-between p-3 cursor-pointer ${
      selected ? 'bg-blue-50 border-l-4 border-blue-600' : 'hover:bg-gray-50'
    }`}
    onClick={onClick}
  >
    <span className="truncate">{conversation.conversationTitle}</span>
    <button
      onClick={(e) => {
        e.stopPropagation();
        onDelete();
      }}
      className="text-gray-400 hover:text-red-600"
    >
      <X size={16} />
    </button>
  </div>
);

export default ConversationItem;
