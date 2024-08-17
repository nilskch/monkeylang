import { useState } from "react";
import Navbar from "./components/Navbar";
import Editor from "./components/Editor";
import Output from "./components/Output";
import ActionBar from "./components/ActionBar";

const App = () => {
  const [code, setCode] = useState<string>('print("Hello, World!")');
  const [output, setOutput] = useState<string>("");

  const handleFormat = () => {};
  const handleRun = () => {
    setOutput(
      " onClickonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonk\nonClickonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClickonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick\nonClick",
    );
  };

  return (
    <div className="flex flex-col h-screen">
      <Navbar />
      <div className="flex-1 flex flex-col gap-3 p-4">
        <ActionBar handleRun={handleRun} handleFormat={handleFormat} />
        <Editor code={code} setCode={setCode} />
        <Output output={output} handleClose={() => setOutput("")} />
      </div>
    </div>
  );
};

export default App;
