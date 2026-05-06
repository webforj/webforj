package com.webforj.data;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * sub bean.
 */
public class AddressBean {

  @NotNull
  private String street;

  @Size(min = 2)
  private String city;

  private String zip;

  public AddressBean() {}

  /**
   * Constructor.
   *
   * @param street the street
   * @param city the city
   * @param zip the zip code
   */
  public AddressBean(String street, String city, String zip) {
    this.street = street;
    this.city = city;
    this.zip = zip;
  }

  public String getStreet() {
    return street;
  }

  public void setStreet(String street) {
    this.street = street;
  }

  public String getCity() {
    return city;
  }

  public void setCity(String city) {
    this.city = city;
  }

  public String getZip() {
    return zip;
  }

  public void setZip(String zip) {
    this.zip = zip;
  }
}
