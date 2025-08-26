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
      className="p-4 border-t bg-white flex flex-col gap-3"
    >
      {/* First row: file + text + send */}
      <div className="flex items-center gap-2">
        {/* File Upload */}
        <label className="cursor-pointer text-gray-500 hover:text-gray-700">
          <Paperclip size={18} />
          <input
            type="file"
            className="hidden"
            onChange={(e) => setFile(e.target.files?.[0] || null)}
          />
        </label>

        {/* Show filename if selected */}
        {file && (
          <span className="text-sm text-gray-600 truncate max-w-[150px]">
            {file.name}
          </span>
        )}

        {/* Text Input */}
        <input
          type="text"
          value={content}
          onChange={(e) => setContent(e.target.value)}
          className="flex-1 border rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
          placeholder="Type a message..."
        />

        {/* Submit */}
        <button
          type="submit"
          className="bg-blue-600 text-white p-2 rounded-lg hover:bg-blue-700 transition"
        >
          <Send size={18} />
        </button>
      </div>

      {/* Second row: Tool Call Buttons */}
      <div className="flex gap-2">
        <button
          type="button"
          onClick={() => toggleToolCall("WebSearch")}
          className={`px-3 py-1 rounded-lg text-sm font-medium border transition ${
            toolCall === "WebSearch"
              ? "bg-blue-600 text-white border-blue-600"
              : "bg-gray-100 text-gray-700 hover:bg-gray-200 border-gray-300"
          }`}
        >
          🌐 Web
        </button>
        <button
          type="button"
          onClick={() => toggleToolCall("Wikipedia")}
          className={`px-3 py-1 rounded-lg text-sm font-medium border transition ${
            toolCall === "Wikipedia"
              ? "bg-green-600 text-white border-green-600"
              : "bg-gray-100 text-gray-700 hover:bg-gray-200 border-gray-300"
          }`}
        >
          📖 Wiki
        </button>
      </div>
    </form>
  );
};

export default MessageInput;
