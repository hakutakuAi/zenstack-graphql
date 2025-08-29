import type { Prisma } from '@prisma/client'

// Base select objects for each model (scalar fields only)
export const USER_BASE_SELECT = {
	id: true,
	createdAt: true,
	updatedAt: true,
	email: true,
	name: true,
	bio: true,
} satisfies Prisma.UserSelect

export const POST_BASE_SELECT = {
	id: true,
	createdAt: true,
	updatedAt: true,
	title: true,
	content: true,
	published: true,
	viewCount: true,
	authorId: true,
} satisfies Prisma.PostSelect

export const COMMENT_BASE_SELECT = {
	id: true,
	createdAt: true,
	content: true,
	postId: true,
	authorId: true,
} satisfies Prisma.CommentSelect

export const CATEGORY_BASE_SELECT = {
	id: true,
	name: true,
	description: true,
} satisfies Prisma.CategorySelect

// Select objects with relations
export const USER_WITH_POSTS_SELECT = {
	...USER_BASE_SELECT,
	posts: {
		select: POST_BASE_SELECT,
	},
} satisfies Prisma.UserSelect

export const USER_WITH_COMMENTS_SELECT = {
	...USER_BASE_SELECT,
	comments: {
		select: COMMENT_BASE_SELECT,
	},
} satisfies Prisma.UserSelect

export const USER_WITH_ALL_RELATIONS_SELECT = {
	...USER_BASE_SELECT,
	posts: {
		select: POST_BASE_SELECT,
	},
	comments: {
		select: COMMENT_BASE_SELECT,
	},
} satisfies Prisma.UserSelect

export const POST_WITH_AUTHOR_SELECT = {
	...POST_BASE_SELECT,
	author: {
		select: USER_BASE_SELECT,
	},
} satisfies Prisma.PostSelect

export const POST_WITH_COMMENTS_SELECT = {
	...POST_BASE_SELECT,
	comments: {
		select: COMMENT_BASE_SELECT,
	},
} satisfies Prisma.PostSelect

export const POST_WITH_ALL_RELATIONS_SELECT = {
	...POST_BASE_SELECT,
	author: {
		select: USER_BASE_SELECT,
	},
	comments: {
		select: COMMENT_BASE_SELECT,
	},
	categories: {
		select: {
			postId: true,
			categoryId: true,
			assignedAt: true,
			post: {
				select: POST_BASE_SELECT,
			},
			category: {
				select: CATEGORY_BASE_SELECT,
			},
		},
	},
} satisfies Prisma.PostSelect

export const COMMENT_WITH_RELATIONS_SELECT = {
	...COMMENT_BASE_SELECT,
	post: {
		select: POST_BASE_SELECT,
	},
	author: {
		select: USER_BASE_SELECT,
	},
} satisfies Prisma.CommentSelect

export const CATEGORY_WITH_POSTS_SELECT = {
	...CATEGORY_BASE_SELECT,
	posts: {
		select: {
			postId: true,
			categoryId: true,
			assignedAt: true,
			post: {
				select: POST_BASE_SELECT,
			},
			category: {
				select: CATEGORY_BASE_SELECT,
			},
		},
	},
} satisfies Prisma.CategorySelect

// CategoryOnPost select objects
export const POST_CATEGORY_BASE_SELECT = {
	postId: true,
	categoryId: true,
	assignedAt: true,
} satisfies Prisma.CategoryOnPostSelect

export const POST_CATEGORY_WITH_RELATIONS_SELECT = {
	...POST_CATEGORY_BASE_SELECT,
	post: {
		select: POST_BASE_SELECT,
	},
	category: {
		select: CATEGORY_BASE_SELECT,
	},
} satisfies Prisma.CategoryOnPostSelect

// Type definitions for select return types
export type UserWithPosts = Prisma.UserGetPayload<{
	select: typeof USER_WITH_POSTS_SELECT
}>

export type UserWithComments = Prisma.UserGetPayload<{
	select: typeof USER_WITH_COMMENTS_SELECT
}>

export type UserWithAllRelations = Prisma.UserGetPayload<{
	select: typeof USER_WITH_ALL_RELATIONS_SELECT
}>

export type PostWithAuthor = Prisma.PostGetPayload<{
	select: typeof POST_WITH_AUTHOR_SELECT
}>

export type PostWithComments = Prisma.PostGetPayload<{
	select: typeof POST_WITH_COMMENTS_SELECT
}>

export type PostWithAllRelations = Prisma.PostGetPayload<{
	select: typeof POST_WITH_ALL_RELATIONS_SELECT
}>

export type CommentWithRelations = Prisma.CommentGetPayload<{
	select: typeof COMMENT_WITH_RELATIONS_SELECT
}>

export type CategoryWithPosts = Prisma.CategoryGetPayload<{
	select: typeof CATEGORY_WITH_POSTS_SELECT
}>

export type PostCategoryWithRelations = Prisma.CategoryOnPostGetPayload<{
	select: typeof POST_CATEGORY_WITH_RELATIONS_SELECT
}>
