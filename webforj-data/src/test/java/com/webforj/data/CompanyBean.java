package com.webforj.data;

import jakarta.validation.constraints.NotNull;

/**
 * Parent bean.
 */
public class CompanyBean {

  @NotNull
  private String name;

  private AddressBean address;

  private String email;

  public CompanyBean() {}

  /**
   * Constructor.
   *
   * @param name the company name
   * @param address the company address
   * @param email the company email
   */
  public CompanyBean(String name, AddressBean address, String email) {
    this.name = name;
    this.address = address;
    this.email = email;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public AddressBean getAddress() {
    return address;
  }

  public void setAddress(AddressBean address) {
    this.address = address;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public String getDisplayName() {
    return name == null ? "" : "[" + name + "]";
  }
}
