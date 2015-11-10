public static class submitTopology_result implements org.apache.thrift.TBase<submitTopology_result, submitTopology_result._Fields>, java.io.Serializable, Cloneable, Comparable<submitTopology_result>   {
  boolean is_set_e();
  int e;
  boolean is_set_ite();
  int ite;
  boolean is_set_aze();
  int aze;
  
  public boolean equals(submitTopology_result o1, submitTopology_result o2) {
    boolean this_present_e = true && o1.is_set_e();
    boolean that_present_e = true && o2.is_set_e();
    if (this_present_e || that_present_e) {
      if (!(this_present_e && that_present_e)) {
        return false;
      }
      if (equals(o1.e, o2.e) == 0) {
        return false;
      }
    }

    boolean this_present_ite = true && o1.is_set_ite();
    boolean that_present_ite = true && o2.is_set_ite();
    if (this_present_ite || that_present_ite) {
      if (!(this_present_ite && that_present_ite)) {
        return false;
      }
      if (equals(o1.ite, o2.ite) == 0) {
        return false;
      }
    }

    boolean this_present_aze = true && o1.is_set_aze();
    boolean that_present_aze = true && o2.is_set_aze();
    if (this_present_aze || that_present_aze) {
      if (!(this_present_aze && that_present_aze)) {
        return false;
      }
      if (equals(o1.aze, o2.aze) == 0) {
        return false;
      }
    }

    return true;
  }
}