  protected class BuildQueueElement implements Comparable<BuildQueueElement> {
    protected IBuildStep step;
    protected int level;

    public BuildQueueElement(IBuildStep _step, int _level) {
      step = _step;
      level = _level;
    }

    public IBuildStep getStep() {
      return step;
    }

    public int getLevel() {
      return level;
    }

    public void setLevel(int _level) {
      level = _level;
    }

    @Override
    public int hashCode() {
      return step.hashCode();
    }

    @Override
    public int compareTo(BuildQueueElement elem) {
      if (elem == null)
        throw new NullPointerException();

      if (elem.getLevel() > level)
        return -1;
      if (elem.getLevel() < level)
        return 1;
      return 0;
    }

}
