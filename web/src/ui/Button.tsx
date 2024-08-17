import React from "react";

const Button = ({
  children,
  onClick,
}: {
  children: React.ReactNode;
  onClick: () => void;
}) => {
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
