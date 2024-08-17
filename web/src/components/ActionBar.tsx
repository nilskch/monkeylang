import Button from "../ui/Button";

type ActionBarProps = {
  handleRun: () => void;
  handleFormat: () => void;
};

const ActionBar = ({ handleRun, handleFormat }: ActionBarProps) => {
  return (
    <div className="flex items-center justify-between">
      <div className="flex items-center gap-4">
        <Button onClick={handleRun}>Run</Button>
        <Button onClick={handleFormat}>Format</Button>
      </div>
      <Button onClick={() => console.log("TODO")}>Examples</Button>
    </div>
  );
};

export default ActionBar;
