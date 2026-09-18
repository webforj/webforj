package com.webforj.devtools.craftforj.source;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The imports a source edit keeps in step with the file.
 *
 * <p>
 * Required imports are present after the edit. Tracked imports are added while the edited file
 * still uses them and removed once it no longer does.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class SourceImports {

  private final Set<String> required = new LinkedHashSet<>();
  private final Set<String> trackedCandidates = new LinkedHashSet<>();
  private final Set<String> trackedUsed = new LinkedHashSet<>();

  /**
   * Gets the live set of imports the edit requires.
   *
   * @return the fully qualified names, open for additions
   */
  public Set<String> getRequired() {
    return required;
  }

  /**
   * Sets the imports whose presence follows their use in the edited file.
   *
   * @param candidates the fully qualified names the edit manages
   * @param used the candidates the edited file still uses
   */
  public void setTracked(Collection<String> candidates, Collection<String> used) {
    trackedCandidates.clear();
    trackedCandidates.addAll(candidates);
    trackedUsed.clear();
    trackedUsed.addAll(used);
  }

  /**
   * Gets every import the edit manages.
   *
   * @return the required and the tracked fully qualified names
   */
  public Set<String> getCandidates() {
    Set<String> candidates = new LinkedHashSet<>(trackedCandidates);
    candidates.addAll(required);

    return candidates;
  }

  /**
   * Gets the managed imports the edited file needs.
   *
   * @return the required names and the tracked names still in use
   */
  public Set<String> getUsed() {
    Set<String> used = new LinkedHashSet<>(trackedUsed);
    used.addAll(required);

    return used;
  }
}
