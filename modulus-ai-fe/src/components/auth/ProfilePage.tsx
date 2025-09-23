import React, { useState, useEffect } from 'react';
import { User, Lock, ArrowLeft, Settings, Mail, Shield, CheckCircle } from 'lucide-react';
import { useNavigate } from 'react-router';
import ChangePasswordModal from './ChangePasswordModal';

const ProfilePage: React.FC = () => {
  const navigate = useNavigate();
  const [userEmail, setUserEmail] = useState('');
  const [showChangePasswordModal, setShowChangePasswordModal] = useState(false);

  useEffect(() => {
    // In a real app, you might want to fetch user data from an API
    // For now, we'll use a placeholder email
    setUserEmail('user@example.com');
  }, []);

  const handleBackToChat = () => {
    navigate('/chat');
  };

  const handleChangePassword = () => {
    setShowChangePasswordModal(true);
  };

  return (
    <div className="min-h-screen flex items-center justify-center p-4 relative overflow-hidden">
      {/* Background Elements */}
      <div className="absolute inset-0 bg-gradient-to-br from-gray-900 via-gray-800 to-gray-900"></div>
      <div className="absolute top-0 left-0 w-full h-full overflow-hidden">
        <div className="absolute -top-40 -right-40 w-80 h-80 bg-gradient-to-br from-blue-600/30 to-purple-800/30 rounded-full blur-3xl"></div>
        <div className="absolute -bottom-40 -left-40 w-80 h-80 bg-gradient-to-tr from-cyan-600/30 to-pink-800/30 rounded-full blur-3xl"></div>
        <div className="absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 w-96 h-96 bg-gradient-to-r from-violet-600/20 to-blue-800/20 rounded-full blur-3xl"></div>
      </div>

      <div className="relative z-10 w-full max-w-2xl">
        {/* Back Button */}
        <div className="mb-6">
          <button
            onClick={handleBackToChat}
            className="flex items-center gap-2 text-gray-400 hover:text-gray-200 transition-colors duration-200 group"
          >
            <ArrowLeft size={20} className="group-hover:-translate-x-1 transition-transform duration-200" />
            <span className="text-sm font-medium">Back to Chat</span>
          </button>
        </div>

        {/* Main Profile Card */}
        <div className="card p-8 animate-fade-in">
          {/* Header */}
          <div className="text-center mb-8">
            <div className="relative mb-6">
              <div className="w-24 h-24 mx-auto bg-gradient-to-br from-blue-500 to-indigo-600 rounded-2xl flex items-center justify-center shadow-lg">
                <User className="text-white" size={40} />
              </div>
              <div className="absolute -top-1 -right-1 w-8 h-8 bg-gradient-to-r from-green-400 to-emerald-500 rounded-full flex items-center justify-center">
                <CheckCircle className="w-5 h-5 text-white" />
              </div>
            </div>
            <h1 className="text-3xl font-bold bg-gradient-to-r from-gray-100 to-gray-300 bg-clip-text text-transparent mb-2">
              Profile Settings
            </h1>
            <p className="text-gray-400 text-sm">
              Manage your account information and security settings
            </p>
          </div>

          {/* Profile Information */}
          <div className="space-y-6">
            {/* User Avatar Section */}
            <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
              <div className="flex items-center gap-4">
                <div className="w-16 h-16 bg-gradient-to-br from-blue-500 to-indigo-600 rounded-xl flex items-center justify-center shadow-lg">
                  <User className="text-white" size={32} />
                </div>
                <div className="flex-1">
                  <h3 className="text-lg font-semibold text-gray-100 mb-1">Profile Avatar</h3>
                  <p className="text-sm text-gray-400">Your account avatar</p>
                </div>
                <button className="btn btn-secondary px-4 py-2 text-sm">
                  Change Avatar
                </button>
              </div>
            </div>

            {/* Email Section */}
            <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
              <div className="flex items-center gap-4">
                <div className="w-12 h-12 bg-gradient-to-br from-cyan-500 to-blue-600 rounded-lg flex items-center justify-center">
                  <Mail className="text-white" size={20} />
                </div>
                <div className="flex-1">
                  <h3 className="text-lg font-semibold text-gray-100 mb-1">Email Address</h3>
                  <p className="text-sm text-gray-400">{userEmail}</p>
                </div>
                <div className="px-3 py-1 bg-green-900/30 text-green-400 text-xs font-medium rounded-full border border-green-500/30">
                  Verified
                </div>
              </div>
            </div>

            {/* Security Section */}
            <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
              <div className="flex items-center gap-4">
                <div className="w-12 h-12 bg-gradient-to-br from-orange-500 to-red-600 rounded-lg flex items-center justify-center">
                  <Shield className="text-white" size={20} />
                </div>
                <div className="flex-1">
                  <h3 className="text-lg font-semibold text-gray-100 mb-1">Password Security</h3>
                  <p className="text-sm text-gray-400">Keep your account secure with a strong password</p>
                </div>
                <button
                  onClick={handleChangePassword}
                  className="btn btn-primary px-4 py-2 text-sm flex items-center gap-2"
                >
                  <Lock size={16} />
                  Change Password
                </button>
              </div>
            </div>

            {/* Settings Section */}
            <div className="bg-gray-800/50 rounded-xl p-6 border border-gray-700/50">
              <div className="flex items-center gap-4">
                <div className="w-12 h-12 bg-gradient-to-br from-purple-500 to-pink-600 rounded-lg flex items-center justify-center">
                  <Settings className="text-white" size={20} />
                </div>
                <div className="flex-1">
                  <h3 className="text-lg font-semibold text-gray-100 mb-1">Account Settings</h3>
                  <p className="text-sm text-gray-400">Manage your account preferences and notifications</p>
                </div>
                <button className="btn btn-secondary px-4 py-2 text-sm">
                  Open Settings
                </button>
              </div>
            </div>
          </div>


          {/* Footer Actions */}
          <div className="mt-8 flex gap-4">
            <button
              onClick={handleBackToChat}
              className="flex-1 btn btn-secondary py-3 px-6 text-sm font-semibold rounded-xl"
            >
              Back to Chat
            </button>
            <button
              onClick={() => {
                // Handle logout
                navigate('/login');
              }}
              className="flex-1 btn bg-red-600 hover:bg-red-700 text-white py-3 px-6 text-sm font-semibold rounded-xl"
            >
              Sign Out
            </button>
          </div>
        </div>
      </div>

      {/* Change Password Modal */}
      <ChangePasswordModal
        isOpen={showChangePasswordModal}
        onClose={() => setShowChangePasswordModal(false)}
      />
    </div>
  );
};

export default ProfilePage;
