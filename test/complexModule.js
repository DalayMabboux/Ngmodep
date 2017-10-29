import { NgModule } from '@angular/core';  
import { CommonModule } from '@angular/common';  
import { FormsModule } from '@angular/forms';

/** Some comment */

const basketHeaderBottomModule = angular.module('basketHeaderBottom', [
    basketModule
])
    .component('basketHeaderBottomComponent', basketHeaderBottomComponent)
    .name;

export {MyComponent}
