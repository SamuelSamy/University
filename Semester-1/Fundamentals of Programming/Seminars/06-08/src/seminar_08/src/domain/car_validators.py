from domain.exceptions import CarValidationException


class CarValidatorES:
    @staticmethod
    def _is_license_valid(self, license_):
        # TODO Implement full validation
        """
        Implement Spanish license plate validation
        @param license_:
        @return: ...
        """
        return len(license_) > 2

    # FIXME Duplicated code across validators, use inheritance to remove it
    def validate(self, car):
        errors = []
        # V1 - All properties are non-empty
        if not self._is_license_valid(car.license_plate):
            errors.append('Invalid license plate')
        if len(car.make) < 3:
            errors.append('Car make should have at least 3 letters')
        if len(car.model) < 3:
            errors.append('Car model should have at least 3 letters')

        if len(errors) > 0:
            raise CarValidationException(errors)


class CarValidatorRO:
    @staticmethod
    def _is_license_valid(self, license):
        # TODO Implement full validation
        """
        Implement Romanian license plate validation
        @param license:
        @return: ...
        """
        return len(license) > 2

    # FIXME Duplicated code across validators, use inheritance to remove it
    def validate(self, car):
        errors = []
        # V1 - All properties are non-empty
        if not self._is_license_valid(car.license_plate):
            errors.append('Invalid license plate')
        if len(car.make) < 2:
            errors.append('Car make should have at least 3 letters')
        if len(car.model) < 2:
            errors.append('Car model should have at least 3 letters')

        if len(errors) > 0:
            raise CarValidationException(errors)
