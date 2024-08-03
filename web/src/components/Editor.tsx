import React from "react";

const Editor = () => {
  const [code, setCode] = React.useState(`import React from 'react';

function App() {
  return (
    <div>
      <h1>Welcome to my app!</h1>
      <p>This is a sample React component.</p>
    </div>
  );
}`);

  return (
    <div className="rounded-lg border flex-1 flex flex-col">
      <div className="flex h-full">
        <div className="bg-background text-foreground font-mono rounded-lg shadow-lg overflow-hidden w-full">
          <div className="flex h-full">
            <div className="bg-gray-100 px-4 py-4 text-right text-gray-500 text-sm leading-6 select-none">
              {Array.from({ length: 20 }, (_, i) => (
                <div key={i}>{i + 1}</div>
              ))}
            </div>
            <div className="flex-1 p-4">
              <textarea
                className="w-full h-full bg-transparent border-none outline-none resize-none text-sm"
                value={code}
                onChange={(e) => setCode(e.target.value)}
                style={{ overflow: "hidden" }}
              />
            </div>
          </div>
        </div>
      </div>
      <div className="flex-1" />
    </div>
  );
};

export default Editor;
