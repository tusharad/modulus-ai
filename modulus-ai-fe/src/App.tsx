import React, { useState, useEffect } from 'react';
import { getCookie } from './services/cookies';
import { BrowserRouter } from 'react-router';
import Router from './router';
import { ThemeProvider } from './contexts/ThemeContext';
import { ApiKeyProvider } from './contexts/ApiKeyContext';
import './App.css';

const App: React.FC = () => {
  const [_, setIsAuthenticated] = useState(false);

  useEffect(() => {
    const token = getCookie('accessToken');
    if (token) {
      setIsAuthenticated(true);
    } 
  }, []);

  return (
    <ThemeProvider>
      <ApiKeyProvider>
        <div className="app-container">
          <BrowserRouter>
            <Router />
          </BrowserRouter>
        </div>
      </ApiKeyProvider>
    </ThemeProvider>
  );
};

export default App;
