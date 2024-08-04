const Output = () => {
  const input = `
    `;
  return null;
  return (
    <div className="bg-background rounded-lg border p-2 flex flex-col gap-2">
      <h2 className="text-lg font-bold">Output</h2>
      <div className="bg-gray-100 rounded-md p-2 flex-2 overflow-auto">
        <pre className="text-sm whitespace-pre-wrap">{input}</pre>
      </div>
    </div>
  );
};

export default Output;
