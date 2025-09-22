import React from 'react';
import { X, MessageSquare } from 'lucide-react';
import type { ConversationRead } from '../../types';

interface Props {
  conversation: ConversationRead;
  selected: boolean;
  onClick: () => void;
  onDelete: () => void;
}

const ConversationItem: React.FC<Props> = ({ conversation, selected, onClick, onDelete }) => (
  <div
    className={`group flex items-center justify-between p-3 cursor-pointer transition-all duration-200 ${
      selected 
        ? 'bg-gradient-to-r from-blue-900/30 to-indigo-900/30 border-l-4 border-blue-400 shadow-sm' 
        : 'hover:bg-gray-800/50 hover:shadow-sm'
    }`}
    onClick={onClick}
  >
    <div className="flex items-center gap-3 flex-1 min-w-0">
      <div className={`w-8 h-8 rounded-lg flex items-center justify-center transition-colors duration-200 ${
        selected 
          ? 'bg-blue-500 text-white' 
          : 'bg-gray-800 text-gray-400 group-hover:bg-blue-900/30 group-hover:text-blue-400'
      }`}>
        <MessageSquare size={14} />
      </div>
      <span className={`truncate text-sm font-medium transition-colors duration-200 ${
        selected ? 'text-white' : 'text-gray-300 group-hover:text-gray-100'
      }`}>
        {conversation.conversationTitle}
      </span>
    </div>
    <button
      onClick={(e) => {
        e.stopPropagation();
        onDelete();
      }}
      className="opacity-0 group-hover:opacity-100 text-gray-500 hover:text-red-400 transition-all duration-200 p-1 rounded hover:bg-red-900/20"
    >
      <X size={14} />
    </button>
  </div>
);

export default ConversationItem;