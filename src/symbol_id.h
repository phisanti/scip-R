// symbol_id.h — declarations for SCIP symbol-ID construction helpers.
// Path sanitizing, SCIP identifier escaping, and local/global symbol-ID
// builders shared by symbol_resolve.cpp; defined in symbol_id.cpp.

#ifndef SCIPR_SYMBOL_ID_H
#define SCIPR_SYMBOL_ID_H

#include <string>

/// Sanitize a file path for use in SCIP symbol IDs.
/// Replaces any character that isn't alphanumeric, underscore, or slash with _.
/// Used only for the path component of local symbol IDs, NOT for symbol names.
///
/// @param s Input string (a file path)
/// @return Sanitized string safe for symbol ID path component
std::string sanitize_path_for_symbol_id(const std::string& s);

/// Escape a symbol name for SCIP identifiers per the SCIP spec.
/// SCIP identifier characters: _ + - $ ASCII letters and digits.
/// Names containing anything else (notably '.' in R) must be backtick-quoted.
/// Internal backticks are escaped by doubling.
///
/// @param name Raw symbol name (e.g., "pgx.createPGX")
/// @return Escaped name (e.g., "`pgx.createPGX`"), or unchanged if no escaping needed
std::string escape_scip_identifier(const std::string& name);

/// Build a local (file-scoped) symbol ID.
/// Local symbols are not resolvable from other files.
/// Format without scope: "local <sanitized_path>#<escaped_name>"
/// Format with scope:    "local <sanitized_path>#<escaped_scope>#<escaped_name>"
///
/// When scope_id is provided (enclosing function name), it creates a 2-hash ID
/// that distinguishes local variables inside functions from top-level file
/// assignments. This is critical for dead code detection to avoid false positives
/// on function-local variables.
///
/// Uses SCIP backtick-quoting for names with special chars (e.g. dots in R)
/// to preserve the original name instead of stripping information.
///
/// @param name Symbol name (e.g., "x", "has.pgx")
/// @param path Relative file path (e.g., "R/utils.R")
/// @param scope_id Optional enclosing function name (e.g., "my_func", "pgx.create")
/// @return Symbol ID string
std::string build_local_symbol_id(
    const std::string& name,
    const std::string& path,
    const std::string& scope_id = ""
);

/// Build a global (package-level) or external symbol ID.
/// Format: "scip-r r <package> <version> <name><suffix>"
///
/// Suffix conventions (matching SCIP standard):
///   - Functions: "()" suffix, e.g., "foo()."
///   - Variables: "." suffix, e.g., "foo."
///   - Classes: "#" suffix, e.g., "Foo#"
///
/// @param name Symbol name
/// @param pkg Package name (e.g., "base", "dplyr")
/// @param version Package version (e.g., "4.3.2")
/// @param kind Symbol kind: "function", "call", "variable", "r6_class", etc.
/// @return Symbol ID string
std::string build_global_symbol_id(
    const std::string& name,
    const std::string& pkg,
    const std::string& version,
    const std::string& kind
);

#endif  // SCIPR_SYMBOL_ID_H
