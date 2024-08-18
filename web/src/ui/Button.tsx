import React from "react";

type ButtonProps = {
  children: React.ReactNode;
  onClick: () => void;
};

const Button = ({ children, onClick }: ButtonProps) => {
  return (
    <button
      onClick={onClick}
      className="bg-black hover:bg-gray-900 text-white font-bold py-1 px-3 rounded"
    >
      {children}
    </button>
  );
};

export default Button;
