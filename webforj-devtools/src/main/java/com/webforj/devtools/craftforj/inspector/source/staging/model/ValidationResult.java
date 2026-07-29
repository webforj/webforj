package com.webforj.devtools.craftforj.inspector.source.staging.model;

import java.util.List;
import java.util.Map;

/**
 * Outcome of validating a set of staged sources together.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ValidationResult {

  private final boolean success;
  private final Map<String, Boolean> verifiedByFile;
  private final List<CompileDiagnostic> errors;

  /**
   * Creates a validation result.
   *
   * @param success whether every staged file is acceptable
   * @param verifiedByFile per file flag telling whether full compile validation ran
   * @param errors the diagnostics that caused a rejection, empty on success
   */
  public ValidationResult(boolean success, Map<String, Boolean> verifiedByFile,
      List<CompileDiagnostic> errors) {
    this.success = success;
    this.verifiedByFile = Map.copyOf(verifiedByFile);
    this.errors = errors == null ? List.of() : List.copyOf(errors);
  }

  /**
   * Checks whether every staged file is acceptable.
   *
   * @return {@code true} when the staged set validated
   */
  public boolean isSuccess() {
    return success;
  }

  /**
   * Tells whether a file went through full compile validation rather than parse only validation.
   *
   * @param path the file path
   * @return {@code true} when the file was compile verified
   */
  public boolean isVerified(String path) {
    return Boolean.TRUE.equals(verifiedByFile.get(path));
  }

  /**
   * Gets the diagnostics that caused a rejection.
   *
   * @return the diagnostics, empty on success
   */
  public List<CompileDiagnostic> getErrors() {
    return errors;
  }
}
