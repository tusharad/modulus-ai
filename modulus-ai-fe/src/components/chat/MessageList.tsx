import React, { useCallback, useEffect, useRef } from 'react';
import type { ChatMessageRead } from '../../types';
import MessageBubble from './MessageBubble';

interface Props {
  messages: ChatMessageRead[];
  loadingMore?: boolean;
  hasMoreMessages?: boolean;
  onLoadMore?: () => void;
}

const MessageList: React.FC<Props> = ({ 
  messages, 
  loadingMore = false, 
  hasMoreMessages = true, 
  onLoadMore 
}) => {
  const scrollContainerRef = useRef<HTMLDivElement>(null);

  const handleScroll = useCallback(() => {
    if (!scrollContainerRef.current || !onLoadMore || loadingMore || !hasMoreMessages) {
      return;
    }

    const { scrollTop } = scrollContainerRef.current;
    
    // Load more when user scrolls near the top (80% threshold)
    if (scrollTop <= 100) {
      onLoadMore();
    }
  }, [onLoadMore, loadingMore, hasMoreMessages]);

  useEffect(() => {
    const scrollContainer = scrollContainerRef.current;
    if (!scrollContainer) return;

    scrollContainer.addEventListener('scroll', handleScroll);
    return () => scrollContainer.removeEventListener('scroll', handleScroll);
  }, [handleScroll]);

  // Auto-scroll to bottom when new messages arrive (except when loading more)
  useEffect(() => {
    if (!loadingMore && scrollContainerRef.current) {
      const scrollContainer = scrollContainerRef.current;
      scrollContainer.scrollTop = scrollContainer.scrollHeight;
    }
  }, [messages.length, loadingMore]);

  return (
    <div 
      ref={scrollContainerRef}
      className="flex-1 overflow-y-auto chat-scroll p-6"
    >
      {messages.length === 0 ? (
        <div className="flex items-center justify-center h-full">
          <div className="text-center">
            <div className="w-16 h-16 mx-auto mb-4 bg-gradient-to-br from-blue-100 to-indigo-100 rounded-2xl flex items-center justify-center">
              <svg className="w-8 h-8 text-blue-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
              </svg>
            </div>
            <h3 className="text-lg font-semibold text-gray-700 mb-2">Start the conversation</h3>
            <p className="text-gray-500 text-sm">Send a message to begin chatting with AI</p>
          </div>
        </div>
      ) : (
        <>
          {loadingMore && hasMoreMessages && (
            <div className="flex justify-center py-4">
              <div className="flex items-center space-x-2 text-gray-400">
                <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-blue-400"></div>
                <span className="text-sm">Loading more messages...</span>
              </div>
            </div>
          )}
          <div className="space-y-1">
            {messages.map((msg) => (
              <MessageBubble key={msg.chatMessagePublicID} message={msg} />
            ))}
          </div>
        </>
      )}
    </div>
  );
};

export default MessageList;
