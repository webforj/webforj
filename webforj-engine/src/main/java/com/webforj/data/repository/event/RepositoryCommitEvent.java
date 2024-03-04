package com.webforj.data.repository.event;

import com.webforj.data.repository.Repository;
import java.util.EventObject;
import java.util.List;

/**
 * An event fired when a repository is refreshed.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RepositoryCommitEvent<T> extends EventObject {
  private transient Repository<T> repository;
  private transient List<T> commits;
  private transient boolean isSingleCommit;

  /**
   * Creates a new instance of RepositoryCommitEvent.
   *
   * @param source The repository that fired the event.
   * @param commits The list of commits.
   * @param isSingleCommit A flag to indicate if the event is for a single commit.
   */
  public RepositoryCommitEvent(Repository<T> source, List<T> commits, boolean isSingleCommit) {
    super(source);
    this.repository = source;
    this.commits = commits;
    this.isSingleCommit = isSingleCommit;
  }

  /**
   * Gets the repository that fired the event.
   *
   * @return The repository that fired the event.
   */
  public Repository<T> getRepository() {
    return repository;
  }

  /**
   * Gets the list of commits.
   *
   * @return The list of commits.
   */
  public List<T> getCommits() {
    return commits;
  }

  /**
   * Gets the first commit in the list of commits.
   *
   * @return The first commit in the list of commits.
   */
  public T getFirstCommit() {
    return getCommits().get(0);
  }

  /**
   * Gets a flag to indicate if the event is for a single commit.
   *
   * @return A flag to indicate if the event is for a single commit.
   */
  public boolean isSingleCommit() {
    return isSingleCommit;
  }
}
