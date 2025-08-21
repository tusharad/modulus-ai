import React from 'react';
import { Plus, LogOut, ChevronRight, ChevronLeft } from 'lucide-react';
import type { ConversationRead } from '../../types';
import ConversationItem from './ConversationItem';

interface Props {
  conversations: ConversationRead[];
  selectedConversation: ConversationRead | null;
  onSelect: (conv: ConversationRead) => void;
  onDelete: (id: string) => void;
  onNewConversation: () => void;
  onLogout: () => void;
  collapsed: boolean;
  onToggleCollapse: () => void;
}

const ConversationList: React.FC<Props> = ({
  conversations,
  selectedConversation,
  onSelect,
  onDelete,
  onNewConversation,
  onLogout,
  collapsed,
  onToggleCollapse,
}) => (
  <div
    className={`${
      collapsed ? "w-16" : "w-64"
    } bg-white border-r flex flex-col transition-all duration-300`}
  >
    {/* Collapse Toggle */}
    <button
      onClick={onToggleCollapse}
      className="p-2 border-b hover:bg-gray-50 flex justify-center"
    >
      {collapsed ? <ChevronRight size={20} /> : <ChevronLeft size={20} />}
    </button>

    {!collapsed && (
      <>
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
      </>
    )}
  </div>
);

export default ConversationList;
