package com.webforj.devtools.craftforj.inspector.source;

import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * Per-file state shared by the change writers while one file is processed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class WriteContext {

  private final Map<ChangeRequest, UsagePlanner.UsagePlan> plans;
  private final Set<String> requiredImports;
  private final Map<ChangeRequest, String> replacedExpressions = new LinkedHashMap<>();

  WriteContext(Map<ChangeRequest, UsagePlanner.UsagePlan> plans, Set<String> requiredImports) {
    this.plans = plans;
    this.requiredImports = requiredImports;
  }

  Map<ChangeRequest, UsagePlanner.UsagePlan> getPlans() {
    return plans;
  }

  Set<String> getRequiredImports() {
    return requiredImports;
  }

  Map<ChangeRequest, String> getReplacedExpressions() {
    return replacedExpressions;
  }
}
