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
      collapsed ? "w-16" : "w-72"
    } bg-white/90 backdrop-blur-md border-r border-gray-200/50 flex flex-col transition-all duration-300 shadow-sm`}
  >
    {/* Collapse Toggle */}
    <button
      onClick={onToggleCollapse}
      className="p-3 border-b border-gray-200/50 hover:bg-gray-50/50 flex justify-center transition-colors duration-200"
    >
      {collapsed ? <ChevronRight size={20} className="text-gray-500" /> : <ChevronLeft size={20} className="text-gray-500" />}
    </button>

    {!collapsed && (
      <>
        <div className="flex items-center justify-between p-4 border-b border-gray-200/50">
          <div className="flex items-center gap-2">
            <div className="w-2 h-2 bg-blue-400 rounded-full"></div>
            <h2 className="text-lg font-semibold text-gray-800">Conversations</h2>
          </div>
          <button 
            onClick={onNewConversation} 
            className="w-8 h-8 bg-gradient-to-r from-blue-500 to-indigo-600 text-white rounded-lg hover:from-blue-600 hover:to-indigo-700 transition-all duration-200 shadow-sm hover:shadow-md flex items-center justify-center"
          >
            <Plus size={16} />
          </button>
        </div>
        <div className="flex-1 overflow-y-auto chat-scroll">
          {conversations.length === 0 ? (
            <div className="p-4 text-center">
              <div className="w-12 h-12 mx-auto mb-3 bg-gray-100 rounded-xl flex items-center justify-center">
                <svg className="w-6 h-6 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                </svg>
              </div>
              <p className="text-sm text-gray-500">No conversations yet</p>
              <p className="text-xs text-gray-400 mt-1">Create one to get started</p>
            </div>
          ) : (
            conversations.map((conv) => (
              <ConversationItem
                key={conv.conversationPublicID}
                conversation={conv}
                selected={selectedConversation?.conversationPublicID === conv.conversationPublicID}
                onClick={() => onSelect(conv)}
                onDelete={() => onDelete(conv.conversationPublicID)}
              />
            ))
          )}
        </div>
      </>
    )}
  </div>
);

export default ConversationList;
