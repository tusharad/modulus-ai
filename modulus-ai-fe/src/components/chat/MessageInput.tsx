import React, { useState } from 'react';
import { Send } from 'lucide-react';

interface Props {
  onSend: (content: string) => void;
}

const MessageInput: React.FC<Props> = ({ onSend }) => {
  const [content, setContent] = useState('');

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!content.trim()) return;
    onSend(content.trim());
    setContent('');
  };

  return (
    <form onSubmit={handleSubmit} className="p-4 border-t bg-white flex items-center gap-2">
      <input
        type="text"
        value={content}
        onChange={(e) => setContent(e.target.value)}
        className="flex-1 border rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
        placeholder="Type a message..."
      />
      <button
        type="submit"
        className="bg-blue-600 text-white p-2 rounded-lg hover:bg-blue-700 transition"
      >
        <Send size={18} />
      </button>
    </form>
  );
};

export default MessageInput;
