package com.webforj.data.transformation.transformer;

import java.time.Duration;
import java.util.UUID;

/**
 * Holds a task with a UUID id, a Duration estimate and a note.
 */
public class TaskBean {
  private UUID id = UUID.fromString("123e4567-e89b-12d3-a456-426614174000");
  private Duration estimate = Duration.ofMinutes(90);
  private String note = "Review";

  /**
   * Gets the id.
   *
   * @return the id
   */
  public UUID getId() {
    return id;
  }

  /**
   * Sets the id.
   *
   * @param id the id
   */
  public void setId(UUID id) {
    this.id = id;
  }

  /**
   * Gets the estimate.
   *
   * @return the estimate
   */
  public Duration getEstimate() {
    return estimate;
  }

  /**
   * Sets the estimate.
   *
   * @param estimate the estimate
   */
  public void setEstimate(Duration estimate) {
    this.estimate = estimate;
  }

  /**
   * Gets the note.
   *
   * @return the note
   */
  public String getNote() {
    return note;
  }

  /**
   * Sets the note.
   *
   * @param note the note
   */
  public void setNote(String note) {
    this.note = note;
  }
}
