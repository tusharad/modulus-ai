import React, { useState, useEffect } from 'react';
import { getCookie } from './services/cookies';
import { BrowserRouter } from 'react-router';
import Router from './router';

// Main App Component
const App: React.FC = () => {
  const [, setIsAuthenticated] = useState(false);

  useEffect(() => {
    // Check if user is already logged in
    const token = getCookie('accessToken');
    if (token) {
      setIsAuthenticated(true);
    }
  }, []);

return (
    <BrowserRouter>
      <Router />
    </BrowserRouter>
  );
};

export default App;
