import React from 'react';

const GeneratingReplyAnimation: React.FC = () => {
  return (
    <div className="flex items-center space-x-2">
      <span className="text-gray-500 text-sm font-medium">Generating reply</span>
      <div className="flex space-x-1">
        <div 
          className="w-2 h-2 bg-blue-400 rounded-full animate-bounce" 
          style={{ animationDelay: '0ms', animationDuration: '1.4s' }}
        ></div>
        <div 
          className="w-2 h-2 bg-blue-400 rounded-full animate-bounce" 
          style={{ animationDelay: '200ms', animationDuration: '1.4s' }}
        ></div>
        <div 
          className="w-2 h-2 bg-blue-400 rounded-full animate-bounce" 
          style={{ animationDelay: '400ms', animationDuration: '1.4s' }}
        ></div>
      </div>
    </div>
  );
};

export default GeneratingReplyAnimation;
