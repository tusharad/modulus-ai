// Cookie utilities
export const setCookie = (name: string, value: string, days: number = 7) => {
    const expires = new Date(Date.now() + days * 864e5).toUTCString();
    document.cookie = `${name}=${encodeURIComponent(value)}; expires=${expires}; path=/; SameSite=Strict`;
  };
  
export const getCookie = (name: string): string | null => {
    return document.cookie.split('; ').find(row => row.startsWith(name + '='))?.split('=')[1] ? 
      decodeURIComponent(document.cookie.split('; ').find(row => row.startsWith(name + '='))?.split('=')[1] || '') : null;
  };
  
export const deleteCookie = (name: string) => {
    document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;`;
  };