package Models;

import Interfaces.IGradeModel;

public class GradeModel implements IGradeModel {
    private Double grade;
    private Integer userPoints;
    private Integer totalPoints;

    public GradeModel(Double grade, Integer userPoints, Integer totalPoints) {
        this.grade = grade;
        this.userPoints = userPoints;
        this.totalPoints = totalPoints;
    }

    @Override
    public Double getGrade() {
        return grade;
    }

    @Override
    public Integer getUserPoints() {
        return userPoints;
    }

    @Override
    public Integer getTotalPoints() {
        return totalPoints;
    }
}
