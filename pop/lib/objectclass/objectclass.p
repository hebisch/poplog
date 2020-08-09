/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:			C.all/lib/objectclass/objectclass.p
 > Purpose:			Objectclass: load core sources
 > Author:			Robert John Duncan, Nov 20 1995 (see revisions)
 > Documentation:	HELP * OBJECTCLASS
 > Related Files:	LIB * OBJECTCLASS_HELP
 */
compile_mode:pop11 +strict;

section $-objectclass => pop_oc_version;

;;; Setup the environment
#_IF not(DEF objectclass_dir)
constant objectclass_dir = sys_fname_path(popfilename);
#_ENDIF
uses objectclass_help;

constant pop_oc_version = 10202;

lconstant CHATTY =
	pop_debugging == true and
	not(isdefined("uses_lib_idents") and iscaller(valof("uses_lib_idents")));

if CHATTY then
	printf(';;; Compiling ObjectClass (Version %p.%p.%p)\n', [%
		pop_oc_version div 10000,
		( pop_oc_version mod 10000 ) div 100,
		pop_oc_version mod 100
	%]);
endif;

define lconstant loadfile(path);
	if CHATTY then loadwarning(path) endif;
	pop11_compile(path);
enddefine;

;;; Set flag to say whether we're being loaded from Popc
lvars i = 0, mode = false;
while caller(i) and not(caller_valof("pop_pas_mode", i) ->> mode) do
	i + 1 -> i;
endwhile;
constant OBJECTCLASS_IN_POPC = mode;

;;; standard libraries
uses applynum;
uses typespec_utils;

;;; O/C runtime libraries
lvars file, dir = objectclass_dir dir_>< 'rt/';
for file in
	pdtolist(sys_file_match('*.p', dir, false, false))
do
	loadfile(dir dir_>< sys_fname_name(file));
endfor;

;;; O/C sources
lvars file, dir = objectclass_dir dir_>< 'src/';
for file in [
	'preferences.p'
	'indirect.p'
	'utils.p'
	'readutils.p'
	'derive.p'
	'globals.p'
	'inheritance.p'
	'phantoms.p'
	'tmpvecs.p'
	'slots.p'
	'parts.p'
	'sort_classes.p'
	'method_table.p'
	'methods.p'
	'upgrade_method.p'
	'bind_method.p'
	'method_form.p'
	'define_method.p'
	'fail_generic.p'
	'wlist_to_closure.p'
	'wrappers.p'
	'class_construct.p'
	'class_new.p'
	'class_example.p'
	'objectclasses.p'
	'trees.p'
	'net_to_tree.p'
	'plant_utils.p'
	'mono_tree.p'
	'tree_to_pd.p'
	'instantiate.p'
	'relink_method.p'
	'interpreter.p'
	'full_methods.p'
	'class_isa.p'
	'class_attributes.p'
	'objectclass_form.p'
] do
	loadfile(dir dir_>< file);
endfor;

#_IF DEF OBJECTCLASS_IN_POPC
;;; Force generation of all generic linking code at end of file
popc_after_compile_hook <> optimise_objectclass(%"all"%)
	-> popc_after_compile_hook;
#_ENDIF

if CHATTY then
	printf(';;; ObjectClass compiled\n');
endif;

endsection;		/* $-objectclass */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  7 1995
		Changed CHATTY to check for uses_lib_idents
 */
