import React from 'react';
import { Plus, LogOut } from 'lucide-react';
import type { ConversationRead } from '../../types';
import ConversationItem from './ConversationItem';

interface Props {
  conversations: ConversationRead[];
  selectedConversation: ConversationRead | null;
  onSelect: (conv: ConversationRead) => void;
  onDelete: (id: string) => void;
  onNewConversation: () => void;
  onLogout: () => void;
}

const ConversationList: React.FC<Props> = ({
  conversations,
  selectedConversation,
  onSelect,
  onDelete,
  onNewConversation,
  onLogout,
}) => (
  <div className="w-64 bg-white border-r flex flex-col">
    <div className="flex items-center justify-between p-4 border-b">
      <h2 className="text-lg font-semibold">Conversations</h2>
      <button onClick={onNewConversation} className="text-blue-600 hover:text-blue-800">
        <Plus size={20} />
      </button>
    </div>
    <div className="flex-1 overflow-y-auto">
      {conversations.map((conv) => (
        <ConversationItem
          key={conv.conversationPublicID}
          conversation={conv}
          selected={selectedConversation?.conversationPublicID === conv.conversationPublicID}
          onClick={() => onSelect(conv)}
          onDelete={() => onDelete(conv.conversationPublicID)}
        />
      ))}
    </div>
    <button
      onClick={onLogout}
      className="flex items-center justify-center gap-2 p-4 border-t hover:bg-gray-50 text-red-600"
    >
      <LogOut size={18} /> Logout
    </button>
  </div>
);

export default ConversationList;
