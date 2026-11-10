package com.webforj.data.binding;

import jakarta.validation.constraints.Size;

/**
 * A bean whose properties only limit their length.
 */
public class SizedBean {

  @Size(max = 50)
  private String nickname;

  @Size(min = 2, max = 10)
  private String code;

  /**
   * Gets the nickname.
   *
   * @return the nickname
   */
  public String getNickname() {
    return nickname;
  }

  /**
   * Sets the nickname.
   *
   * @param nickname the nickname
   */
  public void setNickname(String nickname) {
    this.nickname = nickname;
  }

  /**
   * Gets the code.
   *
   * @return the code
   */
  public String getCode() {
    return code;
  }

  /**
   * Sets the code.
   *
   * @param code the code
   */
  public void setCode(String code) {
    this.code = code;
  }
}
