const Navbar = () => {
  return (
    <header className="bg-black text-white py-4 px-6 flex items-center justify-between">
      <div className="text-2xl font-bold">Monkeylang Playground</div>
      <nav className="flex items-center gap-4">
        <div className="hover:underline">Github</div>
      </nav>
    </header>
  );
};

export default Navbar;
