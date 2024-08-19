import React from "react";

type ButtonProps = {
  children: React.ReactNode;
  onClick: () => void;
};

const Button = ({ children, onClick }: ButtonProps) => {
  return (
    <button
      onClick={onClick}
      className="inline-flex w-full justify-center gap-x-1.5 rounded-md bg-gray-900 px-3 py-2 text-sm font-semibold text-gray-50 shadow-sm ring-1 ring-inset hover:bg-gray-700"
    >
      {children}
    </button>
  );
};

export default Button;
