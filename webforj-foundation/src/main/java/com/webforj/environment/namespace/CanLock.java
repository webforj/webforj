package com.webforj.environment.namespace;

public interface CanLock {
  public void setLock(String key, long timeout) throws NamespaceVariableLockedException;

  public void removeLock(String key);
}
