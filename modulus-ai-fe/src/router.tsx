import { Routes, Route, Navigate } from 'react-router';
import LoginPage from './components/auth/LoginPage';
import RegisterPage from './components/auth/RegisterPage';
import ChatPage from './components/chat/ChatPage';
import { getCookie } from './services/cookies';

const Router = () => {
  const isAuthenticated = !!getCookie('accessToken');

  return (
    <Routes>
      {/* Auth routes */}
      <Route path="/login" element={<LoginPage />} />
      <Route path="/register" element={<RegisterPage />} />

      {/* Protected Chat route */}
      <Route
        path="/chat"
        element={isAuthenticated ? <ChatPage /> : <Navigate to="/login" />}
      />

      {/* Default redirect */}
      <Route
        path="/"
        element={<Navigate to={isAuthenticated ? '/chat' : '/login'} />}
      />
    </Routes>
  );
};

export default Router;
