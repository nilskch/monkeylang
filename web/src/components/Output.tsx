type OutputProps = {
  output: string;
  handleClose: () => void;
};

const Output = ({ output, handleClose }: OutputProps) => {
  if (!output) {
    return null;
  }

  return (
    <div className="bg-background rounded-lg border p-2 flex flex-col gap-2">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-bold">Output</h2>
        <button
          onClick={handleClose}
          className="text-sm text-gray-500 hover:text-gray-700 mr-2"
        >
          Close
        </button>
      </div>
      <div className="bg-gray-100 rounded-md p-2 flex-2 overflow-auto">
        <pre className="text-sm whitespace-pre-wrap">{output}</pre>
      </div>
    </div>
  );
};

export default Output;
