public static class submitTopology_args implements org.apache.thrift.TBase<submitTopology_args, submitTopology_args._Fields>, java.io.Serializable, Cloneable, Comparable<submitTopology_args>   {

  boolean is_set_name();
  int name;
  boolean is_set_uploadedJarLocation();
  int uploadedJarLocation;
  boolean is_set_jsonConf();
  int jsonConf;
  boolean is_set_topology();
  int topology;

  public boolean equals(submitTopology_args o1, submitTopology_args o2) {
    boolean this_present_name = true && o1.is_set_name();
    boolean that_present_name = true && o2.is_set_name();
    if (this_present_name || that_present_name) {
      if (!(this_present_name && that_present_name)) {
        return false;
      }
      if (equals(o1.name, o2.name) == 0) {
        return false;
      }
    }

    boolean this_present_uploadedJarLocation = true && o1.is_set_uploadedJarLocation();
    boolean that_present_uploadedJarLocation = true && o2.is_set_uploadedJarLocation();
    if (this_present_uploadedJarLocation || that_present_uploadedJarLocation) {
      if (!(this_present_uploadedJarLocation && that_present_uploadedJarLocation)) {
        return false;
      }
      if (equals(o1.uploadedJarLocation, o2.uploadedJarLocation) == 0) {
        return false;
      }
    }

    boolean this_present_jsonConf = true && o1.is_set_jsonConf();
    boolean that_present_jsonConf = true && o2.is_set_jsonConf();
    if (this_present_jsonConf || that_present_jsonConf) {
      if (!(this_present_jsonConf && that_present_jsonConf)) {
        return false;
      }
      if (equals(o1.jsonConf, o2.jsonConf) == 0) {
        return false;
      }
    }

    boolean this_present_topology = true && o1.is_set_topology();
    boolean that_present_topology = true && o2.is_set_topology();
    if (this_present_topology || that_present_topology) {
      if (!(this_present_topology && that_present_topology)) {
        return false;
      }
      if (equals(o1.topology, o2.topology) == 0) {
        return false;
      }
    }

    return true;
  }
}