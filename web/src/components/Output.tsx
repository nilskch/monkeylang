const Output = () => {
  return (
    <div className="bg-background rounded-lg border p-4 flex flex-col gap-4">
      <h2 className="text-xl font-bold">Output</h2>
      <div className="bg-gray-100 rounded-md p-4 flex-1 overflow-auto">
        <pre className="text-sm whitespace-pre-wrap">Hello, World!</pre>
      </div>
    </div>
  );
};

export default Output;
