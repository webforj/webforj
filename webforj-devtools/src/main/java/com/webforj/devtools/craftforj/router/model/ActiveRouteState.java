package com.webforj.devtools.craftforj.router.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Active route state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ActiveRouteState {

  private String currentPath;
  private List<String> activeRouteIds = new ArrayList<>();
  private Map<String, String> params = new HashMap<>();
  private Map<String, String> queryParams = new HashMap<>();
  private String fragment;

  /**
   * Gets the current resolved path.
   *
   * @return the current path
   */
  public String getCurrentPath() {
    return currentPath;
  }

  /**
   * Sets the current resolved path.
   *
   * @param currentPath the current path
   */
  public void setCurrentPath(String currentPath) {
    this.currentPath = currentPath;
  }

  /**
   * Gets the list of route IDs currently rendered.
   *
   * @return the active route IDs
   */
  public List<String> getActiveRouteIds() {
    return activeRouteIds;
  }

  /**
   * Sets the list of route IDs currently rendered.
   *
   * @param activeRouteIds the active route IDs
   */
  public void setActiveRouteIds(List<String> activeRouteIds) {
    this.activeRouteIds = activeRouteIds;
  }

  /**
   * Gets the current route parameter values.
   *
   * @return the params
   */
  public Map<String, String> getParams() {
    return params;
  }

  /**
   * Sets the current route parameter values.
   *
   * @param params the params
   */
  public void setParams(Map<String, String> params) {
    this.params = params;
  }

  /**
   * Gets the current query parameters.
   *
   * @return the query params
   */
  public Map<String, String> getQueryParams() {
    return queryParams;
  }

  /**
   * Sets the current query parameters.
   *
   * @param queryParams the query params
   */
  public void setQueryParams(Map<String, String> queryParams) {
    this.queryParams = queryParams;
  }

  /**
   * Gets the current URL fragment.
   *
   * @return the fragment or null
   */
  public String getFragment() {
    return fragment;
  }

  /**
   * Sets the current URL fragment.
   *
   * @param fragment the fragment
   */
  public void setFragment(String fragment) {
    this.fragment = fragment;
  }
}
