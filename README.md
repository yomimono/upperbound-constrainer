automagically update a whole bunch of opam files to include an upper bound constraint.  I use it like this:

```bash
check=~/upperbound-constrainer/_build/src/main.native;
for dep in $(echo mirage-types mirage-types-lwt mirage); do
	cd ${HOME}/opam-repository && git reset --hard && git checkout origin/master && git branch -D autoconstrain-${dep} && git checkout origin/master -b autoconstrain-${dep};
	for package in `ls packages`; do
		for version in `ls ~/opam-repository/packages/$package/`; do 
			opam_file=~/opam-repository/packages/$package/$version/opam;
			$check $opam_file 3.0.0 ${dep};
		done;
	done;
	git commit -am "$dep upperbound for 3.0.0 automatically added" && git push -f yomimono autoconstrain-${dep};
done 
```

This program depends on unreleased code in `opam-file-format`, so if you want to use it you'll need to pin that package against https://github.com/ocaml/opam-file-format/commit/5059070c301926491418ffdc1d72f17a97479eb5 or a commit like it.

No warranty expressed or implied; if it breaks, you can keep both halves.
