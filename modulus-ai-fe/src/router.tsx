import { Routes, Route, Navigate } from 'react-router';
import LoginPage from './components/auth/LoginPage';
import RegisterPage from './components/auth/RegisterPage';
import ChatPage from './components/chat/ChatPage';
import ProfilePage from './components/auth/ProfilePage';
import { getCookie } from './services/cookies';
import VerifyEmailPage from "./components/auth/VerifyEmailPage";

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

      {/* Protected Profile route */}
      <Route
        path="/profile"
        element={isAuthenticated ? <ProfilePage /> : <Navigate to="/login" />}
      />

      {/* Default redirect */}
      <Route
        path="/"
        element={<Navigate to={isAuthenticated ? '/chat' : '/login'} />}
      />

    <Route path="/verify-email" element={<VerifyEmailPage />} />

    </Routes>
  );
};

export default Router;
