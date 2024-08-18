import Button from "../ui/Button";

type ActionBarProps = {
  handleRun: () => void;
  handleFormat: () => void;
  handleShare: () => void;
  handleExamples: () => void;
};

const ActionBar = ({
  handleRun,
  handleFormat,
  handleShare,
  handleExamples,
}: ActionBarProps) => {
  return (
    <div className="flex justify-between">
      <div className="flex gap-2">
        <Button onClick={handleRun}>Run</Button>
        <Button onClick={handleFormat}>Format</Button>
      </div>
      <div className="flex gap-2">
        <Button onClick={handleShare}>Share</Button>
        <Button onClick={handleExamples}>Examples</Button>
      </div>
    </div>
  );
};

export default ActionBar;
