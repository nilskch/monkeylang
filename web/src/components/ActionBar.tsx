import Button from "../ui/Button";

type ActionBarProps = {
  handleRun: () => void;
  handleFormat: () => void;
};

const ActionBar = ({ handleRun, handleFormat }: ActionBarProps) => {
  return (
    <div className="flex justify-between">
      <div className="flex gap-2">
        <Button onClick={handleRun}>Run</Button>
        <Button onClick={handleFormat}>Format</Button>
      </div>
      <div className="flex gap-2">
        <Button onClick={() => console.log("TODO")}>Share</Button>
        <Button onClick={() => console.log("TODO")}>Examples</Button>
      </div>
    </div>
  );
};

export default ActionBar;
