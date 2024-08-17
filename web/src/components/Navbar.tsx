const Navbar = () => {
  return (
    <header className="bg-black text-white py-4 px-6 flex items-center justify-between">
      <div className="text-2xl font-bold">Monkeylang Playground</div>
      <nav className="flex items-center gap-4">
        <div
          className="hover:cursor-pointer hover:text-gray-200"
          onClick={() =>
            window.open("https://github.com/nilskch/monkeylang", "__blank")
          }
        >
          Github
        </div>
      </nav>
    </header>
  );
};

export default Navbar;
