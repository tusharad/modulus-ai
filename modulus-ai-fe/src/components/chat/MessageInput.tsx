import React, { useState } from 'react';
import { Paperclip, Send } from 'lucide-react';

interface Props {
  onSend: (content: string, file?: File) => void;
  toolCall: "Wikipedia" | "WebSearch" | null;
  setToolCall: (tool: "Wikipedia" | "WebSearch" | null) => void;
}

const MessageInput: React.FC<Props> = ({ onSend, toolCall, setToolCall }) => {
  const [content, setContent] = useState('');
  const [file, setFile] = useState<File | null>(null);


  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!content.trim() && !file) return; // require message or file
    if(file) 
        console.log("submmit with file", file)
    else 
        console.log("submit without file", file)
    onSend(content.trim(), file || undefined);
    setContent("");
    setFile(null);
  };

  const toggleToolCall = (value: "Wikipedia" | "WebSearch") => {
    setToolCall(toolCall === value ? null : value);
  };

  return (
    <form
      onSubmit={handleSubmit}
      className="p-4 border-t border-gray-200/50 bg-white/80 backdrop-blur-md flex flex-col gap-3"
    >
      {/* First row: file + text + send */}
      <div className="flex items-center gap-3">
        {/* File Upload */}
        <label className="cursor-pointer text-gray-500 hover:text-blue-600 transition-colors duration-200 p-2 rounded-lg hover:bg-blue-50">
          <Paperclip size={18} />
          <input
            type="file"
            className="hidden"
            onChange={(e) => setFile(e.target.files?.[0] || null)}
          />
        </label>

        {/* Show filename if selected */}
        {file && (
          <div className="flex items-center gap-2 bg-blue-50 text-blue-700 px-3 py-1 rounded-lg text-sm">
            <span className="truncate max-w-[150px]">{file.name}</span>
            <button
              type="button"
              onClick={() => setFile(null)}
              className="text-blue-500 hover:text-blue-700"
            >
              ×
            </button>
          </div>
        )}

        {/* Text Input */}
        <input
          type="text"
          value={content}
          onChange={(e) => setContent(e.target.value)}
          className="flex-1 input px-4 py-3 text-sm border-2 border-gray-200 focus:border-blue-500 focus:ring-4 focus:ring-blue-100 transition-all duration-200"
          placeholder="Type a message..."
        />

        {/* Submit */}
        <button
          type="submit"
          className="w-10 h-10 bg-gradient-to-r from-blue-500 to-indigo-600 text-white rounded-xl hover:from-blue-600 hover:to-indigo-700 transition-all duration-200 shadow-sm hover:shadow-md flex items-center justify-center disabled:opacity-50"
          disabled={!content.trim() && !file}
        >
          <Send size={16} />
        </button>
      </div>

      {/* Second row: Tool Call Buttons */}
      <div className="flex gap-2">
        <button
          type="button"
          onClick={() => toggleToolCall("WebSearch")}
          className={`px-3 py-2 rounded-xl text-sm font-medium border-2 transition-all duration-200 ${
            toolCall === "WebSearch"
              ? "bg-blue-500 text-white border-blue-500 shadow-sm"
              : "bg-white text-gray-700 hover:bg-blue-50 hover:text-blue-700 border-gray-200 hover:border-blue-300"
          }`}
        >
          🌐 Web Search
        </button>
        <button
          type="button"
          onClick={() => toggleToolCall("Wikipedia")}
          className={`px-3 py-2 rounded-xl text-sm font-medium border-2 transition-all duration-200 ${
            toolCall === "Wikipedia"
              ? "bg-green-500 text-white border-green-500 shadow-md"
              : "bg-white text-gray-700 hover:bg-green-50 hover:text-green-700 border-gray-200 hover:border-green-300"
          }`}
        >
          📖 Wikipedia
        </button>
      </div>
    </form>
  );
};

export default MessageInput;
