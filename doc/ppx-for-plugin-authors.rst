**********************
PPX for plugin authors
**********************

This section describes how to use ``ppxlib`` for PPX plugin authors.

Metaquot
--------

``metaquot`` is a PPX plugin that helps you write PPX plugins. It lets you write AST node values
using the actual corresponding OCaml syntax instead of building them with the more verbose AST types
or ``Ast_builder``.

To use metaquot you need to add it to the list of preprocessor for your PPX plugin:

.. code:: scheme

          (library
           (name my_plugin_lib)
           (preprocess (pps ppxlib.metaquot)))

Metaquot exposes different extensions. The extension you should use depends on the type of AST node
you're trying to write. You can use the following extensions with the following syntax:
- ``expr`` for ``Parsetree.expression``: ``[%expr 1 + 1]``
- ``pat`` for ``Parsetree.pattern``: ``[%pat? ("", _)]``
- ``type`` for ``Parsetree.core_type``: ``[%type: int -> string]``
- ``stri`` for ``Parsetree.structure_item``: ``[%stri let a = 1]``
- ``sigi`` for ``Parsetree.signature_item``: ``[%sigi: val i : int]``
- ``str`` and ``sig`` respectively for ``Parsetree.structure`` and ``Parsetree.signature``. They use
  similar syntax to the ``_item`` extensions above as they are just a list of such items.

If you consider the first example ``[%expr 1 + 1]``, metaquot will actually expand it into:

.. code:: ocaml

          {
            pexp_desc =
              (Pexp_apply
                 ({
                    pexp_desc = (Pexp_ident { txt = (Lident "+"); loc });
                    pexp_loc = loc;
                    pexp_attributes = []
                  },
                   [(Nolabel,
                      {
                        pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                        pexp_loc = loc;
                        pexp_attributes = []
                      });
                   (Nolabel,
                     {
                       pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                       pexp_loc = loc;
                       pexp_attributes = []
                     })]));
            pexp_loc = loc;
            pexp_attributes = []
          }

For this to compile you need the AST types to be in the scope so you should always use metaquot
where ``Ppxlib`` is opened.
You'll also note that the generated node expects a ``loc : Location.t`` value to be available. The
produced AST node value and every other nodes within it will be located to ``loc``. You should make
sure ``loc`` is the location you want for your generated code when using metaquot.

Using only these extensions you can only produce constant/static AST nodes. Metaquot as a solution
for that as well: anti-quotation. You can use anti-quotation to insert any expression representing
an AST node, including dynamically generated nodes inside a metaquot extension point.

Consider the following example:

.. code:: ocaml

          let with_suffix_expr ~loc s =
            let dynamic_node = Ast_builder.Default.estring ~loc s in
            [%expr [%e dynamic_node] ^ "some_fixed_suffix"]

The ``with_suffix_expr`` function will create an ``expression`` which is the concatenation of the
``s`` argument and the fixed suffix. Ie ``with_suffix_expr "some_dynamic_stem"`` is equivalent to
``[%expr "some_dynamic_steme" ^ "some_fixed_suffix"]``.

The syntax for anti-quotation depends on the type of the node you wish to insert:
- ``e`` to anti-quote values of type ``Parsetree.expression``: ``[%expr 1 + [%e some_expr_node]]``
- ``p`` to anti-quote values of type ``Parsetree.pattern``:
  ``[%pat? (1, [%p some_pat_node]]``
- ``t`` to anti-quote values of type ``Parsetree.core_type``:
  ``[%type: int -> [%t some_core_type_node]]``
- ``m`` to anti-quote values of type ``Parsetree.module_expr`` or ``module_type``:
  ``[%expr let module M = [%m some_module_expr_node]]`` or
  ``[%sigi: module M : [%m some_module_type_node]]``
- ``i`` to anti-quote values of type ``Parsetree.structure_item`` or ``signature_item``:
  ``[%str let a = 1 [%%i some_structure_item_node]]`` or
  ``[%sig: val a : int [%%i some_signature_item_node]]``

As you may have noticed, you can anti-quote expressions which type differs from the type of the
whole metaquot extension point. Eg you can write:

.. code:: ocaml
          let structure_item = [%stri let [%p some_pat] : [%t some_type] = [%e some_expr]]


