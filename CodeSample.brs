' Created Date: 10/28/2020
' Author: Dinesh
' Comments: Below are some of the utility, validation and clone functions I created during my previous tenure 
'			creating streaming channel using Roku BrightScript.
'			These are re-creations of my work and I have not had the opportunity to test them.

'***************************************************************************************************************
'												Utility functions
'***************************************************************************************************************

'description Returns the focused node (if any) of a given parent node and returns a reference to it.
'param node {object} - The parent node from which to begin the search. Only descendatns of this node are searched.
'return {object} - The node that currently possesses the focus or invalid.
function getFocusedNode(node as object) as object
	if (node.hasfocus())
		'node has focus return the node.
		return node
	else
		' check if node is in the focus chain.
		if(node.isInFocusChain())
			'find the child that has focus. 
			for each child in node.getChildren(node.getChildCount(), 0)
				'examine each child and find out the one in focus chain
				if (child.isInFocusChain())
					'found the child in focus chain, now let's go in and find out where the focus is on this child.
					return getFocusedNode(child)
				end if
			end for
		end if
		'node neither has focus nor is in focus chain. return invalid.
		return invalid
	end if
end function

'description - Replaces the sequence placeholder with the values provided in the params object
'param sourceString {string} - A string that may contain placeholders of the form {0} or {key}
'param params {dynamic} - Can be a string, array or associative array
'return {string} - The original string, with placeholders substituted.
'sample - FormatString("This is {0} test", "a") - This is a test
'sample - FormatString("This {1} {0} test ", "a") - This {1} a test
'sample - FormatString("This {1} {0} test", ["a", "is"]) - This is a test
'sample - FormatString("https://{baseurl}/discovery/{endpoint}/{version}/", {"baseurl": "mydomain.com", "endpoint": "metadata", "version": "v1"}) - https://mydomain.com/discovery/metadata/v1/
function FormatString(sourceString as string, params as dynamic) as string
	paramsType = type(params)
	if paramsType = "roAssociativeArray"
		'if paramsType is roAssociativeArray then find each key in the format {key} and replace with value
		for each item in params.items()
			sourceString = sourceString.replace("{"+item.key+"}", item.value.tostr())
		end for
	else
		if paramsType = "String" or paramsType = "roString"
			'if paramsType is string then use it to replace {0}
			sourceString = sourceString.replace("{0}", params)
		else if paramsType = "array" or paramsType = "roArray"
			'if paramsType is array then loop through the array and replace the values
			for i = 0 to params.count()-1
				if instr(1, sourceString, "{"+i.tostr()+"}") <> 0 then sourceString = sourceString.replace("{"+i.tostr()+"}", params[i].tostr())
			end for
		else if paramsType <> "<uninitialized>" and paramsType <> "invalid" and tpi <> "roinvalid" then
			'If this fails then identify the paramsType and add a condition to handle it.
			sourceString = sourceString.replace("{0}", params.tostr())
		end if
	end if
	return sourceString
end function

'***************************************************************************************************************
'											Validation functions
'***************************************************************************************************************

'description - Validates if the requested path on the given object is valid and matches the value provided.
'param obj {dynamic} - Object on which the path should be validated. Only Nodes and AAs are valid values.
'param path {dynamic} - String representing the desired path
'param leafValidationFunction {dynamic} - function ref for the desired validation to be performed on the leaf if it is reached.
'						if a valid function is not passed then default isValidObject validation is performed
'return {boolean}
'samples -
'		obj = {a: {ab: {abc: {abcd: {abcde: "11111", flag: true }}}}}
'		ValidatePath(obj,"a.ab.abc.abcd.abcde", isValidString) should return true
'		ValidatePath(obj,"a.ab.abc.abcd.abcdef", isValidString) should return false
'		ValidatePath(obj,"a.ab.abc.abcd.flag", isTrue) should return true
function ValidatePath(obj, path, leafValidationFunction = invalid) as boolean
	if isCompositeType(obj) = false then return false
	if isValidString(path) = false then return false
	pathargs = path.split(".")
	currObj = obj
	puIndex = pathargs.count() - 2	'we need to traverse to the last branch, not to the leaf; hence count() - 2
	for i = 0 to puIndex
		if isCompositeType(currObj[pathargs[i]]) = false then return false 'further traversing can be done only for nodes or AAs
		currObj = currObj[pathargs[i]]
	end for
	if isFunction(leafValidationFunction) then
		return leafValidationFunction(currObj[pathargs[puIndex+1]])
	else
		return isValidObject(currObj[pathargs[puIndex+1]])
	end if
end function

function isCompositeType(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "roassociativearray" or objType = "rosgnode" or objType = "node"
end function

function isValidObject(obj) as boolean
	return NOT isInvalidObject(obj)
end function

function isInvalidObject(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "invalid" or objType = "roinvalid" or objType = "<uninitialized>"
end function

function isFunction(obj) as boolean
	return Lcase(type(obj)) = "function" or Lcase(type(obj)) = "rofunction"
end function

function isString(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "string" or objType = "rostring"
end function

function isNotString(obj) as boolean
	return not isString(obj)
end function

function isValidString(obj) as boolean
	if isString(obj) = true then
		return len(obj.Trim()) > 0
	else
		return false
	end if
end function

function isInvalidOrEmptyString(obj) as boolean
	return NOT isValidString(obj)
end function

function isPositiveNumber(obj) as boolean
	if isNumericType(obj) = true then
		return obj > 0
	else
		return false
	end if
end function

function isNumericType(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "integer" or objType = "float" or objType = "double" or objType = "longinteger" or objType = "roint" or objType = "rointeger" or objType = "rofloat" or objType = "rodouble" or objType = "rolonginteger"
end function

function isInteger(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "integer" or objType = "roint" or objType = "rointeger"
end function

function isArray(obj) as boolean
	objType = Lcase(type(obj))
	return objectType = "array" or objType = "roarray" or objType = "rolist"
end function

function isNonEmptyArray(obj) as boolean
	if isArray(obj) = true then
		return obj.Count() > 0
	else
		return false
	end if
end function

function isAssocArray(obj) as boolean
	return Lcase(type(obj)) = "roassociativearray"
end function

function isEmptyAA(obj) as boolean
	return isAssocArray(obj) and obj.keys().count() = 0
end function

function isNonEmptyAA(obj) as boolean
	return isAssocArray(obj) and obj.Count() > 0
end function

function isNode(obj) as boolean
	return Lcase(type(obj)) = "rosgnode"
end function

function isBoolean(obj) as boolean
	objType = Lcase(type(obj))
	return objType = "boolean" or objType = "roboolean"
end function

function isTrue(obj) as boolean
	if isBoolean(obj) then
		return obj
	else
		return false
	end if
end function

'***************************************************************************************************************
'												Clone function
'***************************************************************************************************************
'description - Clones a given node and its children based on the inputs specified
'param obj {dynamic} - the object to be cloned.
'param isDeepCopy {boolean} - flag to indicate if children of the node should be cloned as well
'param returnDepth {int} - works in conjunction with isDeepCopy. Specifies the number of levels children to be cloned
'						0 - Entire node tree will be cloned. 
'						1 - Only current level will be cloned. same effect as passing isDeepCopy = false
'						2 or more - Clones the specified depth of children.
'
'NOTE: There is now a native function clone(isDeepCopy as Boolean) as Object under ifSGNodeDict interface.
'		But this function allows for more flexibility in how deep the node tree should be cloned.
function CloneNode(obj, isDeepCopy = false, returnDepth = 1) as object
	retObj = invalid
	if isNode(obj) then
		retObj = CreateObject("roSGNode", type(obj))
		fields = obj.getFields()
		fields.id = fields.id + "_Cloned"
		retObj.setFields(fields)
		if isDeepCopy = true then
			if returnDepth >= 2 then
				for each child in obj.getChildren(obj.getChildCount(), 0)
					retObj.appendChild(CloneNode(child, isDeepCopy, returnDepth - 1))
				end for
			else if returnDepth = 0 then
				for each child in obj.getChildren(obj.getChildCount(), 0)
					retObj.appendChild(CloneNode(child, isDeepCopy, returnDepth))
				end for
			end if
		end if 
	end if
	return retObj
end function