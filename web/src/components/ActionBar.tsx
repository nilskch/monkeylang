import Button from "../ui/Button";
import Dropdown from "../ui/Dropdown";

type ActionBarProps = {
  handleRun: () => void;
  handleFormat: () => void;
  handleShare: () => void;
  handleExamples: (value: string) => void;
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
        <Dropdown
          title="Examples"
          items={[
            { title: "Hello World!", value: "hello_world" },
            { title: "Variables", value: "variables" },
            { title: "If-Else", value: "if_else" },
            { title: "Fibonacci", value: "fibonacci" },
            { title: "Arrays + Maps", value: "arrays_with_maps" },
          ]}
          handleClick={handleExamples}
        />
      </div>
    </div>
  );
};

export default ActionBar;
